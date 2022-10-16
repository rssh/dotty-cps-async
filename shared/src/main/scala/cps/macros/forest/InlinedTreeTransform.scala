// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021, 2022
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._
import scala.collection.immutable.HashMap


trait InlinedTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._

  sealed trait InlinedBindingRecord

  case class InlinedFunBindingRecord(newSym: Symbol, 
                                     cpsTree: CpsTree, 
                                     oldValDef: ValDef, 
                                     newResultType: TypeRepr) extends InlinedBindingRecord

  case class InlinedValBindingRecord(newSym: Symbol, 
                                     cpsTree: CpsTree, 
                                     oldValDef: ValDef,
                                     ) extends InlinedBindingRecord

  case class InlinedBindingsRecord(changes: HashMap[Symbol, InlinedBindingRecord], 
                                   newBindings: List[Definition], 
                                   awaitVals: List[ValDef])
  
  def runInlined(origin: Inlined): CpsTree =
    if (cpsCtx.flags.debugLevel >= 15) then
        cpsCtx.log(s"Inlined, origin=${safeShow(origin)}")  
    val monad = cpsCtx.monad.asTerm
    val s0 = InlinedBindingsRecord(HashMap.empty, List.empty, List.empty)
    val funValDefs = origin.bindings.zipWithIndex.foldLeft(s0){ (s, xi) =>
       val (x,i) = xi
       var debug = false
       x match
          case vx@ValDef(name,tpt,Some(rhs)) =>
              checkLambdaDef(rhs) match
                 case Some(lambda) => 
                    lambda match
                      case Lambda(params, body) =>
                         val cpsBinding = runRoot(body)
                         val resultType = vx.tpt.tpe match
                             case AppliedType(fun, args) => args.last
                             case _ => body.tpe.widen
                         if (cpsBinding.isAsync) then
                            val lambdaTree = new AsyncLambdaCpsTree(lambda, params, cpsBinding, resultType)
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, lambdaTree.rtpe,  vx.symbol.flags, Symbol.noSymbol)
                            val rLambda = lambdaTree.rLambda.changeOwner(newSym)
                            val newValDef = ValDef(newSym, Some(rLambda))
                            // check:
                            val bindingRecord = InlinedFunBindingRecord(newSym, lambdaTree, vx, resultType) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            )
                         else if (cpsBinding.isChanged) then
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, tpt.tpe,  vx.symbol.flags, Symbol.noSymbol)
                            val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => resultType)
                            val newValDef = ValDef(newSym, cpsBinding.syncOrigin.map(body =>
                               Lambda(newSym, mt, 
                                 //(owner,args)=> TransformUtil.substituteLambdaParams(params,args,body,owner).changeOwner(owner)
                                 (owner,args)=> TransformUtil.substituteLambdaParams(params,args,body,owner)
                               )
                            ))
                            val bindingRecord = InlinedFunBindingRecord(newSym, CpsTree.pure(newValDef.rhs.get), vx, resultType) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            ) 
                         else 
                            // unchanged
                            s.copy(newBindings = vx::s.newBindings)
                      case _ => // impossible
                         s.copy(newBindings = vx::s.newBindings)
                 case None => 
                    val cpsRhs = try {
                            runRoot(rhs)
                      } catch {
                         case ex: MacroError =>
                           report.warning(s"error during transformation of valdef in inline, tpt=${tpt.show}\n, rhs=${rhs.show}\n, ex=${ex}", posExprs(rhs))
                           throw ex
                      }
                    cpsRhs.syncOrigin match
                      case None =>
                         val newName = if (debug) name+"DEBUG" else name
                         val newSym = Symbol.newVal(Symbol.spliceOwner, newName, cpsRhs.rtpe,  vx.symbol.flags, Symbol.noSymbol)
                         val newValDef = ValDef(newSym, Some(cpsRhs.transformed.changeOwner(newSym)))

                         if (cpsRhs.isLambda) {
                            val resType = TypeRepr.of[F].appliedTo(cpsRhs.otpe.widen)
                            val bindingRecord =  InlinedFunBindingRecord(newSym, cpsRhs, vx, resType)
                            s.copy(newBindings = newValDef::s.newBindings,
                                   changes = s.changes.updated(vx.symbol, bindingRecord)
                                  )
                         } else  {
                            // here awaits usage are internal  (we just use)
                            val monadContext = cpsCtx.monadContext.asTerm
                     
                            val awaitValSym = Symbol.newVal(Symbol.spliceOwner, name+"$await", tpt.tpe,  
                                                            vx.symbol.flags, Symbol.noSymbol)
                            val awaitVal = ValDef(awaitValSym, Some(
                                   generateAwaitFor(Ref(newSym), cpsRhs.otpe).changeOwner(awaitValSym)
                            ))
                            val bindingRecord =  InlinedValBindingRecord(awaitValSym, cpsRhs, vx) 
                            s.copy(newBindings = newValDef::s.newBindings,
                                   changes = s.changes.updated(vx.symbol, bindingRecord),
                                   awaitVals = awaitVal :: s.awaitVals
                            )
                         }
                      case Some(term) =>
                         if (cpsRhs.isChanged) {
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, vx.tpt.tpe,  vx.symbol.flags, Symbol.noSymbol)
                            val newValDef = ValDef(newSym, Some(term.changeOwner(newSym)))
                            val bindingRecord =  InlinedValBindingRecord(newSym, cpsRhs, vx) 
                            s.copy(newBindings = newValDef::s.newBindings,
                                   changes = s.changes.updated(vx.symbol, bindingRecord)
                            )
                         } else {
                            s.copy(newBindings = vx::s.newBindings)
                         }
          case nonValDef => 
               s.copy(newBindings = x::s.newBindings)
    }  
    var usedAwaitVals = Set.empty[Symbol]
    val bodyWithoutAwaits =
      if (!funValDefs.changes.isEmpty) {
        val transformer = new TreeMap() {

             override def transformTerm(term:Term)(owner: Symbol): Term =
               try
                 transformTermInternal(term)(owner)
               catch
                 case ex: MacroError =>
                   if (cpsCtx.flags.debugLevel > 0) then
                     report.warning(s"failed term: ${term}")
                   throw ex

             def transformTermInternal(term:Term)(owner: Symbol): Term =
               // TODO: implements other methods then apply, i.e. anThen, compose, etc
               term match
                 case Apply(TypeApply(Select(obj@Ident(name),"apply"),targs),args) =>
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding) =>
                              binding match
                                case funBinding: InlinedFunBindingRecord =>
                                  val newArgs = args.map(a => this.transformTerm(a)(owner))
                                  val newApply = Select.unique(Ref(funBinding.newSym),"apply")
                                  val changed = Apply(TypeApply(newApply,targs),newArgs)
                                  if (funBinding.cpsTree.isAsync) then
                                     generateAwaitFor(changed, changed.tpe)
                                  else
                                     changed
                                case _ =>
                                  super.transformTerm(term)(owner)
                           case None =>
                              super.transformTerm(term)(owner)
                 case Apply(Select(obj@Ident(name),"apply"),args) =>
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding: InlinedFunBindingRecord) =>
                              val newArgs = args.map(a => this.transformTerm(a)(owner))
                              val newApply = Select.unique(Ref(binding.newSym),"apply")
                              val changed = Apply(newApply,newArgs)
                              if (binding.cpsTree.isAsync) then
                                 generateAwaitFor(changed, changed.tpe)
                              else
                                 changed
                           case _ =>
                              super.transformTerm(term)(owner)
                 case obj@Ident(name) => 
                        funValDefs.changes.get(obj.symbol) match
                           case Some(vbinding) =>
                             vbinding match
                               case binding: InlinedFunBindingRecord =>
                                 if (binding.cpsTree.isAsync) then
                                   // inline themself
                                   //   low priority todo: think about escaping of work duplication if inlining.
                                   //TODO: currently inline flag is not set in proxy. 
                                   val inlineProxies = binding.oldValDef.symbol.flags.is(Flags.Inline)
                                   val rhs = binding.oldValDef.rhs.get
                                   checkLambdaDef(rhs) match
                                     case Some(Lambda(params, body)) =>
                                        val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => binding.newResultType)
                                        Lambda(Symbol.spliceOwner, mt, 
                                              (owner,args) => 
                                                  (if inlineProxies then
                                                      TransformUtil.substituteLambdaParams(params,args,body,owner)
                                                   else
                                                      Apply(Select.unique(Ref(binding.newSym),"apply"),
                                                                 args.map(_.asInstanceOf[Term]))
                                                  ).changeOwner(owner)
                                        )
                                     case _ =>
                                       throw MacroError("Lambda in rhs expected",posExprs(rhs))
                                 else
                                   Ref(binding.newSym)
                               case binding: InlinedValBindingRecord =>
                                 usedAwaitVals = usedAwaitVals + binding.newSym
                                 Ref(binding.newSym)
                           case None =>
                              super.transformTerm(term)(owner)
                 case _ =>
                        super.transformTerm(term)(owner)
        }
        transformer.transformTerm(origin.body)(Symbol.spliceOwner)
      } else {
        origin.body
      }
    val awaitVals = funValDefs.awaitVals.filter(x => usedAwaitVals.contains(x.symbol))
    val body  = 
      if (!awaitVals.isEmpty) {
             bodyWithoutAwaits.changeOwner(Symbol.spliceOwner) match
                 case block@Block(statements, last) => 
                     TransformUtil.prependStatementsToBlock(awaitVals,block)
                 case other => Block(awaitVals, other)
      } else 
             bodyWithoutAwaits 
    
    
    if (cpsCtx.flags.debugLevel >= 15) then
        cpsCtx.log(s"runInline, body=${body}")
        cpsCtx.log(s"runInline, newBindings=${funValDefs.newBindings.map(TransformUtil.safeShow(_)).mkString("\n")}")
        funValDefs.changes.foreach{ b =>
           cpsCtx.log(s"fubValDef changes binding: ${b}")
        } 
    val cpsBody = runRoot(body)
    if (origin.bindings.isEmpty) then
       cpsBody
    else
       InlinedCpsTree(origin, funValDefs.newBindings.reverse,  cpsBody)


  def checkLambdaDef(term:Term):Option[Term] =
     term match
        case Block(List(),expr) => checkLambdaDef(expr)
        case lt@Lambda(params,body) => Some(lt)
        case Inlined(call,binding,body) => checkLambdaDef(body)
        case _ => None


  def generateAwaitFor(term: Term, tpe:TypeRepr): Term =
   val monad = cpsCtx.monad.asTerm
   val monadContext = cpsCtx.monadContext.asTerm
   Apply(
      Apply(
         TypeApply(
            Ref(awaitSymbol),
            List(Inferred(TypeRepr.of[F]), Inferred(tpe), Inferred(TypeRepr.of[F]) )
         ),
         List(term)
      ),
      List(monad, monadContext)
   )


object InlinedTreeTransform:


  def run[F[_]:Type,T:Type, C<:CpsMonadContext[F]:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T,C],
                         inlinedTerm: qctx1.reflect.Inlined): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     val tmpCCType = summon[Type[C]]
     class Bridge(tc:TransformationContext[F,T,C]) extends
                                                    TreeTransformScope[F,T,C]
                                                    with TreeTransformScopeInstance[F,T,C](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType
         implicit val ccType: quoted.Type[C] = tmpCCType

         def bridge(): CpsExpr[F,T] =
            val origin = inlinedTerm.asInstanceOf[quotes.reflect.Inlined]
            runInlined(origin).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }



