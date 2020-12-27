// CPS Transform for tasty inlined
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._
import scala.collection.immutable.HashMap


trait InlinedTreeTransform[F[_], CT]:

  thisTreeTransform: TreeTransformScope[F,CT] =>

  import qctx.reflect._

  case class InlinedBindingRecord(newSym: Symbol, cpsTree: CpsTree, oldValDef: ValDef)

  case class InlinedBindingsRecord(changes: HashMap[Symbol, InlinedBindingRecord], newBindings: List[Definition])
  
  def runInlined(origin: Inlined): CpsTree =
    val s0 = InlinedBindingsRecord(HashMap.empty, List.empty)
    val funValDefs = origin.bindings.zipWithIndex.foldLeft(s0){ (s, xi) =>
       val (x,i) = xi
       x match
          case vx@ValDef(name,tpt,Some(rhs)) =>
              checkLambdaDef(rhs) match
                 case Some(lambda) => 
                    lambda match
                      case Lambda(params, body) =>
                         val cpsBinding = runRoot(body, TransformationContextMarker.InlinedBinding(i))
                         if (cpsBinding.isAsync) {
                            val lambdaTree = new AsyncLambdaCpsTree(lambda, params, cpsBinding, body.tpe)
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, lambdaTree.rtpe,  vx.symbol.flags, Symbol.noSymbol)
                            val rLambda = lambdaTree.rLambda.changeOwner(newSym)
                            val newValDef = ValDef(newSym, Some(rLambda))
                            val bindingRecord = InlinedBindingRecord(newSym, lambdaTree, vx) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            )
                         } else if (cpsBinding.isChanged) {
                            val newSym = Symbol.newVal(Symbol.spliceOwner, name, tpt.tpe,  vx.symbol.flags, Symbol.noSymbol)
                            // generate new lambda with changed body
                            val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => body.tpe.widen)
                            val newValDef = ValDef(newSym, cpsBinding.syncOrigin.map(body =>
                               Lambda(Symbol.spliceOwner, mt, 
                                 (owner,args)=> TransformUtil.substituteLambdaParams(params,args,body,owner).changeOwner(owner)
                               )
                            ))
                            val bindingRecord = InlinedBindingRecord(newSym, CpsTree.pure(newValDef.rhs.get), vx) 
                            s.copy(
                               changes = s.changes.updated(vx.symbol, bindingRecord),
                               newBindings = newValDef::s.newBindings
                            ) 
                         } else {
                            s.copy(newBindings = vx::s.newBindings)
                         }
                      case _ => // impossible
                              s.copy(newBindings = vx::s.newBindings)
                 case None => 
                      s.copy(newBindings = vx::s.newBindings)
          case _ => 
               s.copy(newBindings = x::s.newBindings)
    }
    val body =
      if (!funValDefs.changes.isEmpty) {
        val monad = cpsCtx.monad.asTerm
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
                              val newArgs = args.map(a => this.transformTerm(a)(owner))
                              val newApply = Select.unique(Ref(binding.newSym),"apply")
                              val changed = Apply(TypeApply(newApply,targs),newArgs)
                              if (binding.cpsTree.isAsync) then
                                 Apply(Apply(TypeApply(Ref(awaitSymbol),List(TypeTree.of[F])),List(changed)),List(monad))
                              else
                                 changed
                           case None =>
                              super.transformTerm(term)(owner)
                 case Apply(Select(obj@Ident(name),"apply"),args) =>
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding) =>
                              val newArgs = args.map(a => this.transformTerm(a)(owner))
                              val newApply = Select.unique(Ref(binding.newSym),"apply")
                              val changed = Apply(newApply,newArgs)
                              if (binding.cpsTree.isAsync) then
                                 Apply(Apply(TypeApply(Ref(awaitSymbol),List(TypeTree.of[F])),List(changed)),List(monad))
                              else
                                 changed
                           case None =>
                              super.transformTerm(term)(owner)
                 case obj@Ident(name) => 
                        funValDefs.changes.get(obj.symbol) match
                           case Some(binding) =>
                              if (binding.cpsTree.isAsync) then
                                  // inline themself
                                  //   low priority todo: think about escaping of work duplication if inlining.
                                  //TODO: currently inline flag is not set in proxy. 
                                  val inlineProxies = binding.oldValDef.symbol.flags.is(Flags.Inline)
                                  val rhs = binding.oldValDef.rhs.get
                                  checkLambdaDef(rhs) match
                                     case Some(Lambda(params, body)) =>
                                        val mt = MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => body.tpe.widen)
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
                           case None =>
                              super.transformTerm(term)(owner)
                 case _ =>
                        super.transformTerm(term)(owner)
        }
        transformer.transformTerm(origin.body)(Symbol.spliceOwner)
      } else {
        origin.body
      }
    if (cpsCtx.flags.debugLevel >= 15) then
        funValDefs.changes.foreach{ b =>
           cpsCtx.log(s"fubValDef changes binding: ${b}")
        } 
    val cpsBody = runRoot(body, TransformationContextMarker.InlinedBody)
    if (origin.bindings.isEmpty) then
       cpsBody
    else
       InlinedCpsTree(origin, funValDefs.newBindings.reverse,  cpsBody)


  def checkLambdaDef(term:Term):Option[Term] =
     term match
        case Block(List(),expr) => checkLambdaDef(expr)
        case lt@Lambda(params,body) => Some(lt)
        case _ => None


object InlinedTreeTransform:


  def run[F[_]:Type,T:Type](using qctx1: Quotes)(cpsCtx1: TransformationContext[F,T],
                         inlinedTerm: qctx1.reflect.Inlined): CpsExpr[F,T] = {

     val tmpFType = summon[Type[F]]
     val tmpCTType = summon[Type[T]]
     class Bridge(tc:TransformationContext[F,T]) extends
                                                    TreeTransformScope[F,T]
                                                    with TreeTransformScopeInstance[F,T](tc) {

         implicit val fType: quoted.Type[F] = tmpFType
         implicit val ctType: quoted.Type[T] = tmpCTType

         def bridge(): CpsExpr[F,T] =
            val origin = inlinedTerm.asInstanceOf[quotes.reflect.Inlined]
            runInlined(origin).toResult[T]


     }
     (new Bridge(cpsCtx1)).bridge()
  }



