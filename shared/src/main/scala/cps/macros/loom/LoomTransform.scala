package cps.macros.loom

import scala.quoted.*
import scala.util.control.NonFatal

import cps.*
import cps.macros.*
import cps.macros.misc.*
import cps.macros.observatory.*

import cps.macros.forest.TransformUtil

// LoomTransform substitute all awaits in text
//  Note, that this macros can be potentially eliminated completely, by makeing 
// hight-level await a macro call. The price will be possible complexity of await catching,
//  via yet one inline.  
object LoomTransform:


    

    def run[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](f: Expr[T], 
        dm: Expr[CpsMonad[F]], 
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

        val treeMap = new TreeMap() {

          override def transformTerm(term: Term)(owner: Symbol): Term = {
            term match
              case applyTerm@Apply(fun,args) =>
                fun match
                  case funApply@Apply(fun1@TypeApply(obj2,targs2), args1) if obj2.symbol == awaitSymbol =>
                    // catch await early
                    val (awaitable, monadContext) = args match
                      case List(frs, snd) => (frs, snd)
                      case other =>
                        throw MacroError(s"expected that await have two implicit argument, our args:${args}", applyTerm.asExpr)
                    runAwait(applyTerm, args1.head, targs2.head.tpe, awaitable, monadContext)
                  case Select(obj,method) =>
                    handleFunSelect(applyTerm, fun, args, obj, method)(owner)
                  case _ =>
                    super.transformTerm(term)(owner)
              case Lambda(params,body) => super.transformTerm(term)(owner)  // to be before Block
              case block@Block(statements, expr) => 
                if (needVarTransformationForAutomaticColoring) {
                  runBlockWithAutomaticColoring(block)(owner)
                } else {
                  super.transformTerm(term)(owner)
                }
              case _ =>
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
                         withInlineBindings(conv, runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head))
              case conv@Inlined(_,_,
                        Lambda(List(xValDef),
                          Block(List(),Apply(Apply(TypeApply(obj3,targs3),List(x)),args1)))
                   ) if (obj3.symbol == awaitSymbol
                          && xValDef.symbol == x.symbol) =>
                         // transient inlines have no 'Typed' entry
                         //  TODO: handle non-inlined conversion
                         withInlineBindings(conv,runAwait(applyTerm, args.head, targs3.head.tpe, args1.head, args1.tail.head))
              case _ =>
                super.transformTerm(applyTerm)(owner)
          }

          def withInlineBindings(origin: Inlined, tree: Term): Term =
            if (origin.bindings.isEmpty)
               tree
            else
               Inlined.copy(origin)(origin.call, origin.bindings, tree)
    
      
          def runAwait(term: Apply, arg: Term, awaitCpsMonadType: TypeRepr, awaitCpsMonad: Term, awaitCpsMonadContext: Term): Term = {
            if flags.debugLevel >= 10 then
                report.info(s"loom:runAwait, arg=${arg.show}")
            val r = if awaitCpsMonadType <:< TypeRepr.of[F] then
              runMyAwait(term, arg, awaitCpsMonadContext)
            else
              runOtherAwait(term, arg, awaitCpsMonadType, awaitCpsMonad, awaitCpsMonadContext)
            if flags.debugLevel >= 10 then
              report.info(s"loom:runAwait, result=${r}")
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
                  case _ =>
                     (symbolMap, out.appended(ce1))
              }
              val nExpr0 = TransformUtil.changeSymsInTerm(allChangedSymbols,block.expr, owner)
              val nExpr1 = transformTerm(nExpr0)(owner) 
              Block.copy(block)(nStats.toList, nExpr1)
          }

         

          /**
           * return new valdef which holds memoized copy of origin valDef, which should be
           * inserted after origin
           **/
          def runValDefWithAutomaticColoring(valDef: ValDef, valDefOwner: Symbol): Option[ValDef] = {
            TransformUtil.inMonadOrChild[F](valDef.tpt.tpe).flatMap{ upte =>
                println("we are here")
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

        }



        val retval = treeMap.transformTerm(f.asTerm)(Symbol.spliceOwner)
        retval.asExprOf[T]
    } 



end LoomTransform