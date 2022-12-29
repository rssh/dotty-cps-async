
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.misc._



/**
 * 
 * transform 
 *    call of NonLocalReturns.throwReturn to call of NonLocalReturnsAsyncShift.throwReturn
 *    coll of NonLocalReturns.returning to:
 *               - NonLocalReturnsAsyncShift.returning in async case
 *               - NonLocalReturnsAsyncShift.syncReturning in sync case.
 **/
trait NonLocalReturnsTreeTransform[F[_], CT, CC<:CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._


   // NonLocalReturns.throwReturn(res)(using returner)
   def runNonLocalReturnsThrowReturn(throwReturnApplyReturner: Apply, 
                    throwReturnApplyResult: Apply, 
                    arg: Term, 
                    targs: List[TypeTree],
                    returner: qctx.reflect.Term)(owner: Symbol): CpsTree = {

   
      def buildShiftedCall(res:Term):Term = {
         Apply.copy(throwReturnApplyReturner)(
            Apply.copy(throwReturnApplyResult)(   
               TypeApply(
                  Ref(shiftedNonLocalReturnsThrowReturnSym),
                  targs
               ),
               List(res)
            ),
            List(returner)
         )
      }

      val resCpsTree = runRoot(arg)(owner)
      
      // TODO:  check that returner is sync to throw right error
      //val returnerCpsTree = runRoot(returner)(owner)

      resCpsTree.syncOrigin match {
         case Some(res) => 
            CpsTree.pure(owner,buildShiftedCall(res),true)     
         case None =>
            resCpsTree.monadFlatMap( x => buildShiftedCall(x), TypeRepr.of[Nothing] )
      }

   }

   

   def runNonLocalReturnsReturning(applyTerm: Apply, fun:Term, targs:List[TypeTree],args:List[Term])(owner:Symbol):CpsTree = {
      if (cpsCtx.flags.debugLevel >= 10) {
         cpsCtx.log("runNonLocalReturnsReturning")
      }
      val paramsDescriptor = MethodParamsDescriptor(fun)
      val substituteNonFatal = new TreeMap {
         override def transformTree(tree:Tree)(owner:Symbol):Tree = {
               tree match
                  case u@Unapply(fun,implicits,patterns) if fun.symbol == nonFatalUnapplySym =>
                     val nFun = Select.unique(nonFatalAndNotControlThrowableAsyncWrapperCompanion,"unapply")
                     Unapply.copy(u)(nFun,implicits,patterns)
                  case _ =>
                     super.transformTree(tree)(owner)
         }
      }
      val nArgs = substituteNonFatal.transformTerms(args)(owner)
      val argRecords = O.buildApplyArgsRecords(paramsDescriptor, nArgs)(owner)
      // we have one arg which is lambda by definition of returning
      argRecords.head match
         case appRecord: ApplyArgLambdaRecord =>
            appRecord.cpsBody.syncOrigin match
               case None => // async keys.
                  val nTerm = Apply.copy(applyTerm)(
                     Apply(
                        TypeApply.copy(fun)(
                           Ref(shiftedNonLocalReturnsReturningSym),
                           TypeTree.of[F]::targs
                        ),
                        List(Ref.term(nonLocalReturnsSym.companionModule.termRef), cpsCtx.monad.asTerm)
                     ),
                     List(appRecord.generateLambda(true,true))
                  )
                  CpsTree.impure(owner,nTerm,applyTerm.tpe)
               case Some(syncBody) =>
                  if (appRecord.cpsBody.isChanged) then
                     val nTerm = Apply.copy(applyTerm)(
                        TypeApply(
                           Ref(shiftedNonLocalReturnsSyncReturningSym),
                           targs
                        ),
                        List(appRecord.generateLambda(false,true))
                     )
                     CpsTree.pure(owner,nTerm,true)
                  else 
                     CpsTree.pure(owner,applyTerm)
         case _ =>
            throw  MacroError("Invalid argument for NonLocalReturs.returning (should be lambda)", posExpr(applyTerm))  
   }
