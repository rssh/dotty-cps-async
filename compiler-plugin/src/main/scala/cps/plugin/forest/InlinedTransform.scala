package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Symbols.*
import core.Decorators.*
import core.Definitions.*
import core.StdNames
import ast.tpd.*

import cps.plugin.*


object InlinedTransform {


  sealed trait BindingRecord  {

    def origin: MemberDef

    def newBinding: Option[MemberDef]

    def generateFlatMap(tail: CpsTree)(using Context, CpsTopLevelContext): Option[CpsTree]

    def substitute(tree: Tree, treeMap: BinginsTreeMap, ctx:Context)(using CpsTopLevelContext): Option[Tree]

  }



  case class UnchangedBindingRecord(origin: MemberDef) extends BindingRecord {
    def newBinding: Option[MemberDef] = {
      Some(origin)
    }
    def generateFlatMap(tail: CpsTree)(using Context, CpsTopLevelContext): Option[CpsTree] = None
    def substitute(tree: Tree,  treeMap: BinginsTreeMap, ctx: Context)(using CpsTopLevelContext): Option[Tree] = None
  }

  case class SyncChangedBindingRecord(origin: ValDef, newValDef: ValDef) extends BindingRecord {
    def newBinding: Option[MemberDef] = {
      Some(newValDef)
    }
    def generateFlatMap(tail:CpsTree)(using Context, CpsTopLevelContext): Option[CpsTree] = None
    override def substitute(tree: Tree, treeMap: BinginsTreeMap, ctx: Context)(using CpsTopLevelContext): Option[Tree] = {
      given Context = ctx
      if (origin.symbol == newValDef.symbol)
        None
      else
        tree match
          case x: Ident if (x.symbol == origin.symbol) =>
                Some(ref(newValDef.symbol))
          case other => None
    }
  }

  case class AdoptedValDefChange(asyncLambdaValDef: Option[ValDef], adoptedRhs: CpsTree)

  def adoptTailToInternalKind(origin: ValDef, owner: Symbol, tail: CpsTree,  internalKind: AsyncKind)(using Context, CpsTopLevelContext): AdoptedValDefChange = {
    internalKind match
      case AsyncKind.Sync =>
        AdoptedValDefChange(None, tail)
      case AsyncKind.Async(v) =>
        val idSym = Symbols.newSymbol(owner, "xId".toTermName, Flags.EmptyFlags, tail.originType.widen, Symbols.NoSymbol)
        val idVal = ValDef(idSym)
        val idRef = ref(idSym)
        val nextCpsTree = FlatMapCpsTree(origin.rhs, tail.owner, tail,
              FlatMapCpsTreeArgument(Some(idVal), CpsTree.unchangedPure(idRef, tail.owner)))
        adoptTailToInternalKind(origin, owner, nextCpsTree, v)
      case AsyncKind.AsyncLambda(bodyKind) =>
        val lambdaName = (origin.name.toString + "$async").toTermName
        val lambdaValSym = Symbols.newSymbol(owner, lambdaName, Flags.Synthetic, tail.transformedType)
        val lambdaVal = ValDef(lambdaValSym, tail.transformed.changeOwner(tail.owner, lambdaValSym))
        AdoptedValDefChange(Some(lambdaVal), tail)
  }


  case class AsyncChangedBindingRecord(origin: ValDef,
                                       cpsedRhs: CpsTree,
                                       adoptedValDefChange: AdoptedValDefChange
                                      ) extends BindingRecord {


    lazy val asyncLambdaValDef: Option[ValDef] = adoptedValDefChange.asyncLambdaValDef
    lazy val adoptedRhs = adoptedValDefChange.adoptedRhs

    override def newBinding: Option[MemberDef] = {
        asyncLambdaValDef
    }

    override def generateFlatMap(tail: CpsTree)(using Context, CpsTopLevelContext): Option[CpsTree] = {
        val retval = tail.asyncKind match
          case AsyncKind.Sync =>
            MapCpsTree(origin.rhs, adoptedRhs.owner, adoptedRhs,
                                    MapCpsTreeArgument(None, tail))
          case AsyncKind.Async(v) =>
            FlatMapCpsTree(origin.rhs, adoptedRhs.owner, adoptedRhs,
                                    FlatMapCpsTreeArgument(Some(origin), tail))
          case AsyncKind.AsyncLambda(bodyKind) =>
            MapCpsTree(origin.rhs, adoptedRhs.owner, adoptedRhs,
                                    MapCpsTreeArgument(None, tail))
         Some(retval)
    }


    override def substitute(tree: Tree, treeMap: BinginsTreeMap, ctx: Context )(using CpsTopLevelContext): Option[Tree] = {
      given Context = ctx
      asyncLambdaValDef match {
        case Some(nValDef) =>
          tree match {
            case Apply(TypeApply(sel@Select(obj@Ident(name),applyName),targs),args)
              if (obj.symbol == origin.symbol && applyName == "apply".toTermName) =>
                val nArgs = args.map(treeMap.transform(_))
                val nSelect = Select(ref(nValDef.symbol), "apply".toTermName)
                val nTree = Apply(TypeApply(nSelect,targs),nArgs)
                Some(insertAwait(nTree, tree.tpe.widen))
            case Apply(Select(obj@Ident(name),applyName),args)
              if (obj.symbol == origin.symbol && applyName == "apply".toTermName) =>
                val nArgs = args.map(treeMap.transform(_))
                val nSelect = Select(ref(nValDef.symbol), "apply".toTermName)
                val nTree = Apply(nSelect,nArgs)
                Some(insertAwait(nTree, tree.tpe.widen))
            case _ =>
              None
          }
        case None => None
      }
    }

    def insertAwait(tree:Tree, tpe: Type )(using ctx:Context, tctx:CpsTopLevelContext): Tree = {
      val awaitSym = Symbols.requiredMethod("cps.await")
      val awaitTypeApply = ref(awaitSym).appliedToTypes(List(tctx.monadType, tpe, tctx.monadType))
      val identityConversion = Symbols.requiredMethod("cps.CpsMonadConversion.identityConversion")
      val identityConversionTypeApply = TypeApply(ref(identityConversion), List(TypeTree(tctx.monadType)))
      val awaitCall =
        Apply(
          Apply(
            awaitTypeApply,
            List(tree)
          ),
          List(
            tctx.cpsMonadRef,
            identityConversionTypeApply
          )
        ).withSpan(tree.span)
      awaitCall
    }

  }

  class BinginsTreeMap(records: List[BindingRecord])(using CpsTopLevelContext) extends TreeMap {

    override def transform(tree: Tree)(using ctx:Context): Tree = {
      val changed = records.collectFirst( Function.unlift(x => x.substitute(tree,this,ctx)) )
      changed.getOrElse(super.transform(tree))
    }

  }

  object InlinedAsyncCall {

    def unapply(tree:Inlined)(using Context, CpsTopLevelContext): Option[Tree] = {
      // This can depends from the compiler implementation.
      //  Note, that expected naive Inlined(Apply(TypeApply("async"),...) is not here.
      tree.call match
        case id if (id.symbol == Symbols.requiredClass("cps.macros.Async$")) =>
             tree.expansion match
               case InferAsyncArgMonadApplyCall(tree) =>
                 Some(tree)
               case _ =>
                 report.warning(s"InlinedAsyncCall/cps.macro.Asygn: unparsed stuff in expansion:${tree.expansion.show}")
                 report.warning(s"InlinedAsyncCall/cps.macro.Asygn: unparsed tree:${tree.expansion}")
                 //
                 Some(tree.expansion)
        case _ => None
    }

  }

  object InferAsyncArgMonadApplyCall {

    def unapply(tree:Tree)(using Context): Option[Tree] = tree match
      case Inlined(call,Nil,expansion) =>
        unapply(expansion)
      case Apply(TypeApply(sel@Select(obj, method), targs), args) =>
        val monadType = Symbols.requiredClass("cps.CpsMonad")
        val objIsMonad = obj.tpe.baseType(monadType) != NoType
        if (objIsMonad && sel.symbol.name == "apply".toTermName)
          Some(tree)
        else None
      case _ => None

  }

  object ContextLambda {

    def unapply(tree:Tree)(using Context): Option[DefDef] = tree match
      case Inlined(call,Nil,expansion) =>
        unapply(expansion)
      case Block((ddef: DefDef)::Nil, closure: Closure)  if ddef.symbol == closure.meth.symbol =>
        Some(ddef)
      case _ => None

  }

  def apply(inlinedTerm: Inlined, owner: Symbol,  nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
    Log.trace(s"InlinedTransform: inlinedTerm.call=${inlinedTerm.call.show}",nesting)
    Log.trace(s"InlinedTransform: inlinedTerm.call.tree=${inlinedTerm.call}", nesting)
    inlinedTerm match
       case InlinedAsyncCall(tree) =>
          // by definition of async, which return F[T]
          Log.trace(s"InlinedTransform: inlinedTerm.call handled, result=${inlinedTerm.show}",nesting)
          CpsTree.pure(inlinedTerm,owner,inlinedTerm)
       case _ =>
        if (inlinedTerm.bindings.isEmpty) then
            RootTransform(inlinedTerm.expansion, owner, nesting+1)  // TODO: add inlined to reporting ? .withSpan(inlinedTerm.span)
        else
            applyNonemptyBindings(inlinedTerm, owner,  nesting)
  }

  def applyNonemptyBindings(inlinedTerm: Inlined, owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      Log.trace(s"InlineTransform: inlinedTerm=${inlinedTerm.show} owner=${owner.id}, bindings.size=${inlinedTerm.bindings.length}",nesting)

      // transform async binder variables in the form
      // when v is Async(_) [not lambda]
      //   block(v'=cpsTransformed(v),Inlined(...[change v to v']..  ) )
      //   (or,   cpsTransformed(v).flatMap(v' => cpsTransformed(Inlined(...[change v to v']..  )) )
      // when v is asyncLambda:
      //   block(v'=cpsTransformed(v),Inlined(...[change v.apply to v'.apply if this is possible]..  ) )
      // where statements in block are bindings.
      //TODO: implement
      //   Now we just check that there are no async bindings in inlined term.
      val records = inlinedTerm.bindings.map{ b =>
        b match {
          case v: ValDef =>
            //  doesm not change symbol of v.
            //   TODO:  add flag to generate new symbol when we need to gnerate a new function.
            val cpsed = RootTransform(v.rhs,v.symbol, nesting+1)
            cpsed.asyncKind match {
              case AsyncKind.Sync =>
                if (cpsed.isOriginEqSync) then
                  UnchangedBindingRecord(v)
                else
                  SyncChangedBindingRecord(v, cpy.ValDef(v)(rhs = cpsed.unpure.get))
              case AsyncKind.Async(internal) =>
                // TODO: check that v can be inline and generate new record for inl
                if (v.symbol.flags.is(Flags.Inline)) then
                  throw new CpsTransformException(s"inline valdefs are not supported in inlined bindings yet [in TODO]", v.srcPos)
                val adoptedValDefChange = adoptTailToInternalKind(v, owner, cpsed,internal)
                AsyncChangedBindingRecord(v, cpsed, adoptedValDefChange)
              case k@AsyncKind.AsyncLambda(bodyKind) =>
                val adoptedValDefChange = adoptTailToInternalKind(v, owner, cpsed,k)
                AsyncChangedBindingRecord(v, cpsed, adoptedValDefChange)
            }
          case _ =>
            throw new CpsTransformException(s"only valdefs are supported in inlined bindings, we have ${b}", b.srcPos)
        }
      }

      val bindingsTreeMap = new BinginsTreeMap(records)
      val changedExpansion = bindingsTreeMap.transform(inlinedTerm.expansion)

      val newBindings = records.flatMap(_.newBinding)


      val cpsedExpansion = RootTransform(changedExpansion,owner,nesting+1)

      val newInlined = cpsedExpansion.asyncKind match
        case AsyncKind.Sync =>
          if (cpsedExpansion.isOriginEqSync) then
            CpsTree.unchangedPure(inlinedTerm,owner)
          else
            CpsTree.pure(inlinedTerm, owner, Inlined(inlinedTerm.call, newBindings, cpsedExpansion.unpure.get))
        case AsyncKind.Async(v) =>
            CpsTree.impure(inlinedTerm, owner, Inlined(inlinedTerm.call, newBindings, cpsedExpansion.transformed), v)
        case AsyncKind.AsyncLambda(internalKind) =>
            Log.trace(s"InlineTransform: newInlined InlinedCpsTree",nesting)
            val newTerm = Inlined(inlinedTerm.call, newBindings, cpsedExpansion.transformed)
            CpsTree.opaqueAsyncLambda(inlinedTerm, owner, newTerm, internalKind)
            //InlinedCpsTree(inlinedTerm,owner,newBindings,cpsedExpansion)

      Log.trace(s"InlineTransform, newInlined: ${newInlined.show}",nesting)


      val prefixedInlined = records.foldRight(newInlined) { (r, acc) =>
          r.generateFlatMap(acc).getOrElse(acc)
      }

      prefixedInlined
  }


}