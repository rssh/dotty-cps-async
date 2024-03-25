// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019 - 2023
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._


class BlockTransform[F[_]:Type, T:Type, C<:CpsMonadContext[F]:Type](cpsCtx: TransformationContext[F,T,C]):

  import cpsCtx._

  // case Block(prevs,last)
  def run(using qctx: Quotes)(prevs: List[qctx.reflect.Statement], last: qctx.reflect.Term): CpsExpr[F,T] =

     if (cpsCtx.flags.debugLevel >= 10) then
        cpsCtx.log(s"Block transform, last=${TransformUtil.safeShow(last)}")
     val tType = summon[Type[T]]
     val uType = summon[Type[Unit]]
     import qctx.reflect._


     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
       p match
         case d: Definition =>
           d match {
             case v@ValDef(vName,vtt,optRhs) =>
               val valDefExpr = Block(List(v),Literal(UnitConstant())).asExprOf[Unit]
               val nestCtx = cpsCtx.nest(valDefExpr, uType)
               ValDefTransform.fromBlock(using qctx)(nestCtx, v)
             case _ =>
               DefCpsExpr(using qctx)(cpsCtx.monad,Seq(),d, false)
           }
         case t: Term =>
           // TODO: rootTransform
           t.asExpr match
               case '{ $p: tp } =>
                       Async.nestTransform[F,T,C,tp](p, cpsCtx)
               case other =>
                       printf(other.show)
                       throw MacroError(s"can't handle term in block: $other",t.asExpr)
         case i:Import =>
            // Import is not statement - so, it is impossible to create block with import
            //   in macros.
            // From another side - all symbols on this stage are already resolved, so
            //  we can just erase import for our purpose.
            CpsExpr.unit(monad)
         case other =>
            printf(other.show)
            throw MacroError(s"unknown tree type in block: $other",patternCode)
     }
     // last can be async lambda, which is not mapped to cpsExpr
     val rLast = Async.nestTransform(last.asExprOf[T],cpsCtx)
     val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
     // wrap yet in one Expr, to avoid unrolling during append in enclosing block).
     val retval = CpsExpr.wrap(blockResult)

     if (cpsCtx.flags.debugLevel >= 15) then
         cpsCtx.log(s"last.isAsync=${rLast.isAsync}")
         cpsCtx.log(s"blockResult.isAsync=${blockResult.isAsync}")
         cpsCtx.log(s"wrapped.isAsync=${retval.isAsync}")
         if (blockResult.isAsync==false && retval.isAsync==true) {
              cpsCtx.log(s"blockResult=${blockResult}")
              cpsCtx.log(s"blockResult.syncOrigin=${blockResult.syncOrigin}")
         }

     retval







class DefCpsExpr[F[_]:Type](using qctx: Quotes)(
                     monad: Expr[CpsMonad[F]],
                     prev: Seq[ExprTreeGen],
                     definition: quotes.reflect.Definition,
                     changed: Boolean) extends SyncCpsExpr[F, Unit](monad, prev) {

  override def isChanged = changed

  def last(using Quotes): Expr[Unit] = '{ () }

  def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,Unit] =
       if (exprs.isEmpty)
         this
       else
         new DefCpsExpr(using quotes)(monad,exprs ++: prev,definition, changed || exprs.exists(_.isChanged)  )

  def append[A:Type](chunk: CpsExpr[F,A])(using Quotes): CpsExpr[F,A] =
       chunk.prependExprs(Seq(StatementExprTreeGen(using this.qctx)(definition, false))).prependExprs(prev)


}
