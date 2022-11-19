package cps.macros.observatory

import scala.collection.mutable.ArrayBuffer
import scala.quoted.*
import cps.*
import cps.macros.*
import cps.macros.forest.*

trait AutomaticColoringOfEffectsQuoteScope:

  this: ObservatoryFullQuoteScope =>

  import quotes.reflect.*

  case class ValUsage(
             var optValDef: Option[ValDef] = None,
             val inAwaits: ArrayBuffer[Tree] = ArrayBuffer(),
             val withoutAwaits: ArrayBuffer[Tree] = ArrayBuffer(),
             val aliases: ArrayBuffer[ValUsage] = ArrayBuffer()
  ):

     def nInAwaits: Int = inAwaits.length + aliases.map(_.nInAwaits).sum
     def nWithoutAwaits: Int = withoutAwaits.length + aliases.map(_.nWithoutAwaits).sum

     def allInAwaits: Seq[Tree]  = 
       (Seq.empty ++ inAwaits.toSeq  ++ aliases.toSeq.flatMap(_.allInAwaits) )
                 
     def allWithoutAwaits: Seq[Tree] = 
       (Seq.empty ++ withoutAwaits.toSeq ++  aliases.toSeq.flatMap(_.allWithoutAwaits) )

     def definedInside: Boolean = !optValDef.isEmpty

     def reportCases():Unit =
       val firstWithout = allWithoutAwaits.headOption
       val firstWith = allInAwaits.headOption
       for(t <- firstWithout) {
          report.info("async usage", t.pos)
       }
       for(t <- firstWith) {
          report.info("usage with await", t.pos)
       }
                                        


  class AutomaticColoringOfEffects extends Analysis:

    val usageRecords = PerSymbolStorage[ValUsage]()

    def awaitSymbol = 
       Symbol.requiredMethod("cps.await")

    override def visitStart[F[_]:Type](tree: Tree, ctx: ObservationContext[F])(owner: Symbol): Unit =
      tree match
        case v@ValDef(name, vtt, Some(rhs)) => 
            val usageRecord = usageRecords.getOrUpdate(v.symbol, ValUsage())
            usageRecord.optValDef = Some(v)
            rhs match
              case idRhs@Ident(_) =>
                 val parentUsageRecord = usageRecords.getOrUpdate(idRhs.symbol, ValUsage()) 
                 parentUsageRecord.aliases.addOne(usageRecord)
                 // not count await/not await in aliases.
              case _ =>
                 ctx.scheduleVisit(rhs, this)(owner)
        case term@Apply(fun, args) => 
                 // to have the same structure as forest/ApplyTransform for the same patterns
                 checkApply(term, fun, args, List(), ctx, owner)
        case id@Ident(x) =>
                 val usageRecord = usageRecords.getOrUpdate(id.symbol, ValUsage())
                 usageRecord.withoutAwaits.addOne(id)
        case _ => ctx.scheduleChildrenVisit(tree, this)(owner)
        

    def checkApply[F[_]:Type](applyTerm: Term,
                              fun: Term,
                              args: List[Term],
                              tails: List[List[Term]],
                              ctx: ObservationContext[F],
                              owner: Symbol): Unit =
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



    def checkFunSelect[F[_]:Type](applyTerm: Term,
                                  fun: Term,
                                  args: List[Term],
                                  obj: Term,
                                  method: String,
                                  tails: List[List[Term]],
                                  ctx: ObservationContext[F],
                                  owner: Symbol): Unit  =
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
      
                                                    
    def checkAwait[F[_]:Type](term: Term, arg: Term, 
                                  awaitCpsMonadType: TypeRepr, 
                                  awaitCpsMonad: Term,
                                  ctx: ObservationContext[F],
                                  owner: Symbol): Unit =
       arg match
          case v:Ident =>
            val usageRecord = usageRecords.getOrUpdate(v.symbol, ValUsage())
            usageRecord.inAwaits.addOne(term)
          case _ =>
            ctx.scheduleChildrenVisit(term, this)(owner)
            

    override def afterTreeTraverse(flags: AsyncMacroFlags): Unit = 
        usageRecords.foreach{ usageRecord =>
            if (!usageRecord.definedInside && usageRecord.aliases.isEmpty
                                           && usageRecord.nInAwaits > 1)  then
                  val allInAwaits = usageRecord.allInAwaits
                  val frs = allInAwaits.head
                  report.info(s"external variable used with await more than once, val=${usageRecord.optValDef.get.name}", frs.pos)
                  for(a <- allInAwaits) {
                     report.info("await: ", a.pos)
                  }
            else if (usageRecord.definedInside && usageRecord.aliases.isEmpty
                      && usageRecord.inAwaits.isEmpty && usageRecord.withoutAwaits.isEmpty) then
                  val valDef = usageRecord.optValDef.get
                  if ( !(valDef.symbol.flags.is(Flags.Synthetic)) ) then 
                     report.warning(s"unused variable, effect may be lost, val=${usageRecord.optValDef.get.name}", usageRecord.optValDef.get.pos)
     
        }



     

