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

    def generateFlatMap(tail: CpsTree)(using Context): Option[CpsTree]

    def substitute(tree: Tree, treeMap: BinginsTreeMap, ctx:Context)(using CpsTopLevelContext): Option[Tree]

  }



  case class UnchangedBindingRecord(origin: MemberDef) extends BindingRecord {
    def newBinding: Option[MemberDef] = {
      Some(origin)
    }
    def generateFlatMap(tail: CpsTree)(using Context): Option[CpsTree] = None
    def substitute(tree: Tree,  treeMap: BinginsTreeMap, ctx: Context)(using CpsTopLevelContext): Option[Tree] = None
  }

  case class SyncChangedBindingRecord(origin: ValDef, newValDef: ValDef) extends BindingRecord {
    def newBinding: Option[MemberDef] = {
      Some(newValDef)
    }
    def generateFlatMap(tail:CpsTree)(using Context): Option[CpsTree] = None
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

  def adoptTailToInternalKind(origin: ValDef, oldOwner: Symbol, newOwner: Symbol, tail: CpsTree,  internalKind: AsyncKind)(using Context, CpsTopLevelContext): AdoptedValDefChange = {
    internalKind match
      case AsyncKind.Sync =>
        AdoptedValDefChange(None, tail)
      case AsyncKind.Async(v) =>
        val idSym = Symbols.newSymbol(newOwner, "xId".toTermName, Flags.EmptyFlags, tail.originType.widen, Symbols.NoSymbol)
        val idVal = ValDef(idSym)
        val idRef = ref(idSym)
        val nextCpsTree = FlatMapCpsTree(origin.rhs, tail.owner, tail,
              FlatMapCpsTreeArgument(Some(idVal), CpsTree.unchangedPure(idRef, tail.owner)))
        adoptTailToInternalKind(origin, oldOwner, newOwner, nextCpsTree, v)
      case AsyncKind.AsyncLambda(bodyKind) =>
        val lambdaName = (origin.name.toString + "$async").toTermName
        val lambdaValSym = Symbols.newSymbol(newOwner, lambdaName, Flags.Synthetic, tail.transformedType)
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

    def generateFlatMap(tail: CpsTree)(using Context): Option[CpsTree] = {
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

  def apply(inlinedTerm: Inlined, oldOwner: Symbol, newOwner: Symbol, nesting: Int)(using Context, CpsTopLevelContext): CpsTree = {
     if (inlinedTerm.bindings.isEmpty) {
        RootTransform(inlinedTerm.expansion, oldOwner, newOwner, nesting+1)
     } else {
        applyNonemptyBindings(inlinedTerm, oldOwner, newOwner, nesting)
     }
  }

  def applyNonemptyBindings(inlinedTermOldOwner: Inlined, oldOwner: Symbol, newOwner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsTree = {
      Log.trace(s"InlineTransform: inlinedTerm=${inlinedTermOldOwner.show} oldOwner=${oldOwner.id} newOwner=${newOwner.id}, bindings.size=${inlinedTermOldOwner.bindings.length}",nesting)

      val inlinedTerm = inlinedTermOldOwner.changeOwner(oldOwner,newOwner)

      val beforeDefOwners = TransformUtil.collectDefOwners(inlinedTerm).map(x => s"(${x._1.id} owner ${x._2.id})").mkString(",")
      Log.trace(s"InlineTransform: beforeDefOwners: ${beforeDefOwners}", nesting)
      val inlinedTermNewOwner = inlinedTerm.changeOwner(oldOwner, newOwner)
      val afterDefOwners = TransformUtil.collectDefOwners(inlinedTermNewOwner).map(x => s"(${x._1.id} owner ${x._2.id})").mkString(",")
      Log.trace(s"InlineTransform: afterDefOwners: ${afterDefOwners}", nesting)


      // transform async binder variables in the form
      // when v is Async(_) [not lambda]
      //   block(v'=cpsTransformed(v),Inlined(...[change v to v']..  ) )
      //   (or,   cpsTransformed(v).flatMap(v' => cpsTransformed(Inlined(...[change v to v']..  )) )
      // when v is asyncLambda:
      //   block(v'=cpsTransformed(v),Inlined(...[change v.apply to v'.apply if this is possible]..  ) )
      // where statements in block are bindings.
      //TODO: implement
      //   Now we just check that there are no async bindings in inlined term.
      val records = inlinedTermNewOwner.bindings.map{ b =>
        b match {
          case v: ValDef =>
            //  doesm not change symbol of v.
            //   TODO:  add flag to generate new symbol when we need to gnerate a new function.
            val cpsed = RootTransform(v.rhs,v.symbol,v.symbol,nesting+1)
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
                val adoptedValDefChange = adoptTailToInternalKind(v, oldOwner, newOwner, cpsed,internal)
                AsyncChangedBindingRecord(v, cpsed, adoptedValDefChange)
              case k@AsyncKind.AsyncLambda(bodyKind) =>
                val adoptedValDefChange = adoptTailToInternalKind(v, oldOwner, newOwner, cpsed,k)
                AsyncChangedBindingRecord(v, cpsed, adoptedValDefChange)
            }
          case _ =>
            throw new CpsTransformException(s"only valdefs are supported in inlined bindings, we have ${b}", b.srcPos)
        }
      }

      val bindingsTreeMap = new BinginsTreeMap(records)
      val changedExpansion = bindingsTreeMap.transform(inlinedTermNewOwner.expansion)

      val newBindings = records.flatMap(_.newBinding)


      val cpsedExpansion = RootTransform(changedExpansion,newOwner,newOwner,nesting+1)

      val newInlined = cpsedExpansion.asyncKind match
        case AsyncKind.Sync =>
          if (cpsedExpansion.isOriginEqSync) then
            Log.trace(s"InlineTransform  newInlined: unchangedPure: ${inlinedTermNewOwner.show}",nesting)
            CpsTree.unchangedPure(inlinedTermNewOwner,newOwner)
          else
            Log.trace(s"InlineTransform  newInlined: changedPure",nesting)
            CpsTree.pure(inlinedTermNewOwner, newOwner, Inlined(inlinedTermNewOwner.call, newBindings, cpsedExpansion.unpure.get))
        case AsyncKind.Async(v) =>
            Log.trace(s"InlineTransform newInlined: impure, newBindings=${newBindings.map(_.show)}",nesting)
            Log.trace(s"InlineTransform newInlined: expansion = ${cpsedExpansion.show}",nesting)
            Log.trace(s"InlineTransform newInlined: expansion.transformed = ${cpsedExpansion.transformed.show}",nesting)
            CpsTree.impure(inlinedTermNewOwner, newOwner, Inlined(inlinedTermNewOwner.call, newBindings, cpsedExpansion.transformed), v)
        case AsyncKind.AsyncLambda(_) =>
            Log.trace(s"InlineTransform: newInlined InlinedCpsTree",nesting)
            InlinedCpsTree(inlinedTermNewOwner,newOwner,newBindings,cpsedExpansion)

      Log.trace(s"InlineTransform, newInlined: ${newInlined.show}",nesting)


      val prefixedInlined = records.foldRight(newInlined) { (r, acc) =>
          r.generateFlatMap(acc).getOrElse(acc)
      }

      prefixedInlined
  }


}