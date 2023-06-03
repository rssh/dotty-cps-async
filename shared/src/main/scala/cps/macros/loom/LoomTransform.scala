package cps.macros.loom

import scala.quoted.*
import scala.util.control.NonFatal

import cps.*
import cps.macros.*
import cps.macros.misc.*
import cps.macros.common.*
import cps.macros.observatory.*


// LoomTransform substitute all awaits in text
//  Note, that this macros can be potentially eliminated completely, by makeing 
// hight-level await a macro call. The price will be possible complexity of await catching,
//  via yet one inline.  
object LoomTransform:

    def run[F[_]:Type, T:Type, C<:CpsTryMonadContext[F]:Type](f: Expr[T],
        dm: Expr[CpsAsyncMonad[F]], 
        ctx: Expr[C], 
        runtimeApi: Expr[CpsRuntimeAwait[F]],
        flags: AsyncMacroFlags,
        optMemoization: Option[TransformationContext.Memoization[F]],
        observatory: Observatory.Scope#Observatory)(using Quotes):Expr[T] = {
        import quotes.reflect.*

        val awaitSymbol = Symbol.requiredMethod("cps.await")

        val needVarTransformationForAutomaticColoring: Boolean = {
            flags.automaticColoring && 
            { optMemoization match
                 case None =>
                     throw MacroError(s"Memoizaton is not defined for ${Type.show[F]}",f)
                 case Some(memoization) =>
                     memoization.kind != CpsMonadMemoization.Kind.BY_DEFAULT
            }
        } 

        def log(message: String): Unit = {
          if (false) then
            report.info(message)
          else
            println(message)
        }

        val treeMap = new TreeMap() {

          override def transformTerm(term: Term)(owner: Symbol): Term = {
            if flags.debugLevel >= 20 then
              log(s"loom:transformTerm start, term=${term.show}")
              log(s"loom:transformTerm start, tree=${term}")
            term match
              case applyTerm@Apply(fun,args) =>
                if flags.debugLevel >= 20 then
                  log(s"loom:transformTerm/applyTerm:1, term=${term.show}")
                fun match
                  case funApply@Apply(fun1@TypeApply(obj2,targs2), args1) if obj2.symbol == awaitSymbol =>
                    // catch await early
                    val (awaitable, monadContext) = args match
                      case List(frs, snd) => (frs, snd)
                      case other =>
                        throw MacroError(s"expected that await have two implicit argument, our args:${args}", applyTerm.asExpr)
                    runAwait(applyTerm, args1.head, targs2.head.tpe, awaitable, monadContext)(owner)
                  case funApply@Apply(fun1@TypeApply(obj2,targs2), args1) =>
                    if flags.debugLevel >= 20 then
                      log(s"loom:transformTerm/applyTerm finish2 (noSyn), term=${term}, no await")
                      log(s"loom:transformTerm/applyTerm finish2 (noSyn), obj2=${obj2}")
                    super.transformTerm(term)(owner)
                  case Select(obj,method) =>
                    handleFunSelect(applyTerm, fun, args, obj, method)(owner)
                  case _ =>
                    if flags.debugLevel >= 20 then
                      log(s"loom:transformTerm/applyTerm finish1, term=${term}, no await")
                    super.transformTerm(term)(owner)
              case Lambda(params,body) =>  // to be before Block
                if flags.debugLevel >= 20 then
                  log(s"loom:transformTerm/lambda, term=${term.show}")
                super.transformTerm(term)(owner) 
              case block@Block(statements, expr) => 
                if flags.debugLevel >= 20 then
                  log(s"loom:transformTerm/block, term=${term.show}")
                if (needVarTransformationForAutomaticColoring) {
                  runBlockWithAutomaticColoring(block)(owner)
                } else {
                  runBlockCheckDiscards(block)(owner)  
                }
              case _ =>
                if flags.debugLevel >= 20 then
                  log(s"loom:transformTerm: call suoer of ${term}")
                super.transformTerm(term)(owner)
          }

  
          def handleFunSelect(applyTerm:Apply, fun:Term, args:List[Term], obj:Term, methodName: String)(owner: Symbol): Term = {
            obj match
              case conv@Inlined(_,_,
                     Typed(
                        Lambda(List(xValDef),
                          Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1))),
                        cv)
                     ) if (obj3.symbol == awaitSymbol
                          && xValDef.symbol == x.symbol) =>
                         // here we catch await, inserted by implicit conversion.
                         // this code is likey depends from implementation details of a compiler
                         // mb create compiler-level API ?
                         withInlineBindings(conv, runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head)(owner))
              case conv@Inlined(_,_,
                        Lambda(List(xValDef),
                          Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1)))
                   ) if (obj3.symbol == awaitSymbol
                          && xValDef.symbol == x.symbol) =>
                         // transient inlines have no 'Typed' entry
                         //  TODO: handle non-inlined conversion
                         withInlineBindings(conv,runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head)(owner))
              case _ =>
                super.transformTerm(applyTerm)(owner)
          }

          def withInlineBindings(origin: Inlined, tree: Term): Term =
            if (origin.bindings.isEmpty)
               tree
            else
               Inlined.copy(origin)(origin.call, origin.bindings, tree)
    
      
          def runAwait(term: Apply, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, awaitCpsMonadContext: Term)(owner: Symbol): Term = {
            if flags.debugLevel >= 10 then
                log(s"loom:runAwait, arg code=${arg.show}")
                log(s"loom:runAwait, arg tree=${arg}")
            val transformedArg = this.transformTerm(arg)(owner)
            val r = if awaitCpsMonadType <:< TypeRepr.of[F] then
              runMyAwait(term, transformedArg, awaitCpsMonadContext)
            else
              runOtherAwait(term, transformedArg, awaitCpsMonadType, awaitCpsMonad, awaitCpsMonadContext)
            if flags.debugLevel >= 10 then
              log(s"loom:runAwait, result=${r}")
            r
          }
      
          def runMyAwait(applyTerm: Apply, arg: Term, ctx:Term): Term = {
            Apply.copy(applyTerm)(
              Apply.copy(applyTerm)(
                TypeApply(Select.unique(runtimeApi.asTerm,"await"),List(Inferred(applyTerm.tpe.widen))),
                List(arg)
              ),
              List(dm.asTerm, ctx)
            )
          }

          def runOtherAwait(applyTerm: Apply, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, ctx: Term): Term = {
            // TODO: set position of
            //Apply.copy(applyTerm)(Select(runtimeApi.asTerm,"await"), List(arg) )
            val myCpsMonad = dm
            val myCpsMonadTpe = dm.asTerm.tpe.widen
            val myF = TypeRepr.of[F]
            val tTpe = applyTerm.tpe.widen
            val monadConversion = TypeIdent(Symbol.classSymbol("cps.CpsMonadConversion")).tpe
            val taConversion = monadConversion.appliedTo(List(awaitCpsMonadType, TypeRepr.of[F]))
            Implicits.search(taConversion) match
                 case implSuccess: ImplicitSearchSuccess =>
                   //val convertedArg = Apply(Select.unique(implSuccess.tree, "apply"),List(arg))
                   val convertedArg = Apply(TypeApply(Select.unique(implSuccess.tree, "apply"),List(Inferred(applyTerm.tpe.widen))),List(arg))
                   runMyAwait(applyTerm, convertedArg, ctx)
                 case implFailure: ImplicitSearchFailure =>
                   val taConversionPrinted = try {
                     taConversion.show
                   } catch {
                     case NonFatal(ex) =>
                      taConversion.toString
                   }
                   throw MacroError(s"Can't find ${taConversionPrinted}: ${implFailure.explanation}", applyTerm.asExpr)
          }

          def runBlockWithAutomaticColoring(block:Block)(owner: Symbol):Term = {
              val (allChangedSymbols, nStats) = block.statements.foldLeft(
                           (Map.empty[Symbol,Term], IndexedSeq.empty[Statement])){ (s, e) =>
                val (symbolMap, out) = s
                val ce0 = TransformUtil.changeSymsInTree(symbolMap, e, owner).asInstanceOf[Statement]
                val ce1 = transformStatement(ce0)(owner)
                e match
                  case valDef@ValDef(_,_,_) =>
                    runValDefWithAutomaticColoring(valDef, owner) match
                      case None =>
                        (symbolMap, out appended ce1)
                      case Some(nValDef) =>
                        val nSymbolMap = symbolMap.updated(valDef.symbol,Ref(nValDef.symbol))
                        val nOut = out appended ce1 appended nValDef
                        (nSymbolMap, nOut)
                  case t: Term =>
                      (symbolMap, out.appended(runTermWithValueDiscard(t, owner)))
                  case _ =>
                      (symbolMap, out.appended(ce1))
              }
              val nExpr0 = TransformUtil.changeSymsInTerm(allChangedSymbols, block.expr, owner)
              val nExpr1 = transformTerm(nExpr0)(owner) 
              Block.copy(block)(nStats.toList, nExpr1)
          }

         

          /**
           * return new valdef which holds memoized copy of origin valDef, which should be
           * inserted after origin
           **/
          def runValDefWithAutomaticColoring(valDef: ValDef, valDefOwner: Symbol): Option[ValDef] = {
            TransformUtil.inMonadOrChild[F](valDef.tpt.tpe).flatMap{ upte =>
                val analysis = observatory.effectColoring
                val usageRecord = analysis.usageRecords.get(valDef.symbol).getOrElse{
                      throw MacroError(s"Can't find analysis record for usage of ${valDef.symbol}", valDef.rhs.get.asExpr)
                }
                if (usageRecord.nInAwaits > 0 && usageRecord.nWithoutAwaits > 0) then 
                  report.error(s"value ${valDef.symbol} passed in sync and async form at the same time",valDef.pos)
                  usageRecord.reportCases()
                if (usageRecord.nInAwaits == 0) then
                  None
                else 
                  // create second variable and 
                  val memoization = optMemoization.get
                  val mm = memoization.monadMemoization.asTerm
                  val mRhs = memoization.kind match
                    case CpsMonadMemoization.Kind.BY_DEFAULT => Ref(valDef.symbol)
                    case CpsMonadMemoization.Kind.INPLACE => 
                      Apply(TypeApply(Select.unique(mm,"apply"),List(Inferred(upte))),List(Ref(valDef.symbol)))
                    case CpsMonadMemoization.Kind.PURE =>
                      val ff = Apply(TypeApply(Select.unique(mm,"apply"),List(Inferred(upte))),List(Ref(valDef.symbol)))
                      Apply(
                        TypeApply(Select.unique(runtimeApi.asTerm,"await"),List(Inferred(valDef.tpt.tpe))),
                        List(ff)
                      )
                    case CpsMonadMemoization.Kind.DYNAMIC =>
                      val mmClass = TypeIdent(Symbol.classSymbol("CpsMonadMemoization.DynamicAp")).tpe
                      val mmType = mmClass.appliedTo(List(TypeRepr.of[F], upte, valDef.tpt.tpe.widen))
                      Implicits.search(mmType) match
                          case success: ImplicitSearchSuccess =>
                              val ff = Apply(Select.unique(success.tree,"apply"), List(Ref(valDef.symbol)))
                              Apply(
                                TypeApply(Select.unique(runtimeApi.asTerm,"await"),List(Inferred(upte))),
                                List(ff)
                              )
                          case failure: ImplicitSearchFailure =>
                              throw MacroError(s"Can't resolve ${mmType.show}: ${failure.explanation}", valDef.rhs.get.asExpr)
                  val nSym = Symbol.newVal(valDefOwner, valDef.name+"$mem",valDef.tpt.tpe,Flags.Local,Symbol.noSymbol)
                  val nValDef = ValDef(nSym, Some(mRhs.changeOwner(nSym)))            
                  Some(nValDef)
            }
          }

          def runBlockCheckDiscards(block: Block)(owner: Symbol):Block = {
            val nStats = block.statements.foldLeft(IndexedSeq.empty[Statement]){ (s,e) =>
              val e1 = transformStatement(e)(owner)
              e1 match 
                case t:Term =>
                  s.appended(runTermWithValueDiscard(t, owner))
                case _ =>
                  s.appended(e1)  
            }
            val nExpr = transformTerm(block.expr)(owner)
            Block.copy(block)(nStats.toList, nExpr)
          }

          def runTermWithValueDiscard(t:Term, owner: Symbol): Term = {
            if (ValueDiscardHelper.checkValueDiscarded(t,flags)) {
              if (flags.customValueDiscard) {
                ValueDiscardHelper.searchCustomDiscardFor(t) match
                  case sc: ImplicitSearchSuccess =>
                    if sc.tree.tpe <:< TypeRepr.of[cps.AwaitValueDiscard[?,?]] then
                      sc.tree.tpe.asType match
                        case '[AwaitValueDiscard[F,tt]] =>
                           '{  ${runtimeApi}.await[tt](${t.asExprOf[F[tt]]})($ctx) }.asTerm
                        case _ =>   
                           ???
                    else
                        Apply(Select.unique(sc.tree,"apply"),List(t))
                  case sf: ImplicitSearchFailure =>
                    val msg = s"discarding non-unit value without custom discard for $t (${sf.explanation})"
                    if (flags.warnValueDiscard) then
                      report.warning(msg, t.pos)
                    else
                      report.error(msg, t.pos)
                    t
              } else {   // (flags.warnValueDiscard) if we here
                  report.warning(s"discarding non-unit value ${t.show}", t.pos)
                  t
              }
            } else {
              t
            }
          }


        }

        val retval = treeMap.transformTerm(f.asTerm)(Symbol.spliceOwner)
        retval.asExprOf[T]
    } 



end LoomTransform
