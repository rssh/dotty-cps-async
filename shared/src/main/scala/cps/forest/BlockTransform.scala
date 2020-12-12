// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020
package cps.forest

import scala.quoted._

import cps._
import cps.misc._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Block(prevs,last)
  def run(using qctx: Quotes)(prevs: List[qctx.reflect.Statement], last: qctx.reflect.Term): CpsExpr[F,T] =

     if (cpsCtx.flags.debugLevel >= 10) then
        cpsCtx.log(s"Block transform, last=${last.show}")
     val tType = summon[Type[T]]
     val uType = summon[Type[Unit]]
     import qctx.reflect._


     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
       p match
         case d: Definition =>
           d match {
             case v@ValDef(vName,vtt,optRhs) =>
               val valDefExpr = Block(List(v),Literal(UnitConstant())).asExprOf[Unit]
               val nestCtx = cpsCtx.nest(valDefExpr, uType,
                                         TransformationContextMarker.BlockInside(i))
               ValDefTransform.fromBlock(using qctx)(nestCtx, v)
             case _ =>
               DefCpsExpr(using qctx)(cpsCtx.monad,Seq(),d)
           }
         case t: Term =>
           // TODO: rootTransform
           t.asExpr match
               case '{ $p: tp } =>
                       if (checkValueDiscarded(using qctx)(t))
                           // bug in dotty: show cause match error in test
                           // see https://github.com/lampepfl/dotty/issues/9684
                           def safeShow(): String =
                             try
                               quoted.Type.show[tp]
                             catch
                               case ex: Throwable => //ex.printStackTrace()
                               TypeTree.of[tp].toString + " [exception during print]"

                           if (cpsCtx.flags.customValueDiscard)
                             val valueDiscard = TypeIdent(Symbol.classSymbol("cps.ValueDiscard")).tpe
                             val tpe = t.tpe.widen.dealias
                             val tpTree = valueDiscard.appliedTo(tpe)
                             Implicits.search(tpTree) match
                               case sc: ImplicitSearchSuccess =>
                                  val pd = Apply(Select.unique(sc.tree,"apply"),List(t)).asExprOf[Unit]
                                  Async.nestTransform(pd, cpsCtx, TransformationContextMarker.BlockInside(i))
                               case fl: ImplicitSearchFailure =>
                                  val tps = safeShow()
                                  val msg = s"discarding non-unit value without custom discard $tps (${fl.explanation})"
                                  if (cpsCtx.flags.warnValueDiscard)
                                      report.warning(msg, t.pos)
                                  else
                                      report.error(msg, t.pos)
                                  Async.nestTransform(p, cpsCtx, TransformationContextMarker.BlockInside(i))
                           else
                             report.warning(s"discarding non-unit value ${safeShow()}", t.pos)
                             Async.nestTransform(p, cpsCtx, TransformationContextMarker.BlockInside(i))
                       else
                         Async.nestTransform(p, cpsCtx, TransformationContextMarker.BlockInside(i))
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
     val rLast = Async.nestTransform(last.asExprOf[T],cpsCtx,TransformationContextMarker.BlockLast)
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


  def checkValueDiscarded(using Quotes)(t: quotes.reflect.Term): Boolean =
     import quotes.reflect._
     ( (cpsCtx.flags.customValueDiscard || cpsCtx.flags.warnValueDiscard)
      &&
       ( !(t.tpe =:= TypeRepr.of[Unit]) && !(t.tpe =:= TypeRepr.of[Nothing]) )
     )



class DefCpsExpr[F[_]:Type](using qctx: Quotes)(
                     monad: Expr[CpsMonad[F]],
                     prev: Seq[ExprTreeGen],
                     definition: quotes.reflect.Definition) extends SyncCpsExpr[F, Unit](monad, prev) {

  def last(using Quotes): Expr[Unit] = '{ () }

  def prependExprs(exprs: Seq[ExprTreeGen]): CpsExpr[F,Unit] =
       if (exprs.isEmpty)
         this
       else
         new DefCpsExpr(using quotes)(monad,exprs ++: prev,definition)

  def append[A:Type](chunk: CpsExpr[F,A])(using Quotes): CpsExpr[F,A] =
       chunk.prependExprs(Seq(StatementExprTreeGen(using this.qctx)(definition))).prependExprs(prev)


}
