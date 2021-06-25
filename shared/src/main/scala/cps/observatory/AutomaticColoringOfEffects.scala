package cps.observatory

import cps.*
import cps.forest.*
import scala.quoted.*

case class ValUsage(
             var definedInside: Boolean = false,
             var inAwaits: Int = 0,
             var withoutAwaits: Int = 0
           )

class AutomaticColoringOfEffects extends Analysis:

  val usageRecords = PerSymbolStorage[ValUsage]()

  def awaitSymbol(using Quotes) = 
     import quotes.reflect.*
     Symbol.requiredMethod("cps.await")

  override def visitStart[F[_]:Type](using qctx: Quotes)(
                      tree: qctx.reflect.Tree, ctx: ObservationContext[F])(owner: qctx.reflect.Symbol): Unit =
    import qctx.reflect.*
    tree match
      case v@ValDef(name, vtt, Some(rhs)) =>
            val vType = TransformUtil.veryWiden(rhs.tpe).asType   
            vType match
               case '[F[r]] =>
                 val usageRecord = usageRecords.getOrUpdate(v.symbol, ValUsage())
                 usageRecord.definedInside = true
                 ctx.scheduleVisit(rhs, this)(owner)
      case term@Apply(fun, args) => 
                 // to have the same structure as forest/ApplyTransform for the same patterns
                 checkApply(term, fun, args, List(), ctx, owner)
      case id@Ident(x) =>
                 val usageRecord = usageRecords.getOrUpdate(id.symbol, ValUsage())
                 usageRecord.withoutAwaits = usageRecord.withoutAwaits + 1
      case _ => ctx.scheduleChildrenVisit(tree, this)(owner)
        

  def checkApply[F[_]:Type](using qctx: Quotes)(applyTerm: qctx.reflect.Term,
                                                        fun: qctx.reflect.Term,
                                                        args: List[qctx.reflect.Term],
                                                        tails: List[List[qctx.reflect.Term]],
                                                        ctx: ObservationContext[F],
                                                        owner: qctx.reflect.Symbol): Unit =
     import qctx.reflect.*
     fun match
       case TypeApply(obj,targs) =>
            ctx.scheduleChildrenVisit(applyTerm, this)(owner)
       case Select(obj,method) =>
            checkFunSelect(applyTerm, fun, args, obj, method, tails, ctx, owner)
       case Apply(fun1@TypeApply(obj2,targs2), args1) if obj2.symbol == awaitSymbol =>
             // catch await early
            checkAwait(applyTerm, args1.head, targs2.head.tpe, args.head, ctx, owner)
       case Apply(fun1, args1) =>
            checkApply(applyTerm, fun1, args1, args::tails, ctx, owner)
       case _ =>
            ctx.scheduleChildrenVisit(applyTerm, this)(owner)



  def checkFunSelect[F[_]:Type, T:Type](using qctx: Quotes)(applyTerm: qctx.reflect.Term,
                                                        fun: qctx.reflect.Term,
                                                        args: List[qctx.reflect.Term],
                                                        obj: qctx.reflect.Term,
                                                        method: String,
                                                        tails: List[List[qctx.reflect.Term]],
                                                        ctx: ObservationContext[F],
                                                        owner: qctx.reflect.Symbol): Unit  =
     import qctx.reflect.*
     obj match
       case conv@Inlined(_,_,
              Typed(
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1))),
                 cv)
              ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                   checkAwait(applyTerm, args.head, targs3.head.tpe, args1.head, ctx, owner)
       case conv@Inlined(_,_,
                 Lambda(List(xValDef),
                   Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1)))
            ) if (obj3.symbol == awaitSymbol
                   && xValDef.symbol == x.symbol) =>
                  // transient inlines have no 'Typed' entry
                  //  TODO: handle non-inlined conversion
                  checkAwait(applyTerm, args.head, targs3.head.tpe, args1.head, ctx, owner)
       case _ => ctx.scheduleChildrenVisit(applyTerm, this)(owner)
      
                                                    
  def checkAwait[F[_]](using qctx:Quotes)(term: qctx.reflect.Term, arg: qctx.reflect.Term, 
                                  awaitCpsMonadType: qctx.reflect.TypeRepr, 
                                  awaitCpsMonad: qctx.reflect.Term,
                                  ctx: ObservationContext[F],
                                  owner: qctx.reflect.Symbol): Unit =
     import qctx.reflect.*
     arg match
        case v:Ident =>
            val usageRecord = usageRecords.getOrUpdate(v.symbol, ValUsage())
            usageRecord.inAwaits = usageRecord.inAwaits + 1
        case _ =>
            ctx.scheduleChildrenVisit(term, this)(owner)
            




