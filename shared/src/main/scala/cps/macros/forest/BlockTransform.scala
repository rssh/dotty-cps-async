// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019, 2020, 2021
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
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
                                  val pd = {
                                    if sc.tree.tpe <:< TypeRepr.of[cps.AwaitValueDiscard[?,?]] then
                                      buildAwaitValueDiscardExpr(using qctx)(sc.tree, p)
                                    else
                                      Apply(Select.unique(sc.tree,"apply"),List(t)).asExprOf[Unit]
                                  }
                                  Async.nestTransform(pd, cpsCtx)
                               case fl: ImplicitSearchFailure =>
                                  val tps = safeShow()
                                  val msg = s"discarding non-unit value without custom discard $tps (${fl.explanation})"
                                  if (cpsCtx.flags.warnValueDiscard)
                                      report.warning(msg, t.pos)
                                  else
                                      report.error(msg, t.pos)
                                  Async.nestTransform(p, cpsCtx)
                           else
                             report.warning(s"discarding non-unit value ${safeShow()}", t.pos)
                             Async.nestTransform(p, cpsCtx)
                       else
                         Async.nestTransform(p, cpsCtx)
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


  def checkValueDiscarded(using Quotes)(t: quotes.reflect.Term): Boolean =
     import quotes.reflect._
     ( (cpsCtx.flags.customValueDiscard || cpsCtx.flags.warnValueDiscard)
      &&
       ( !(t.tpe =:= TypeRepr.of[Unit]) && !(t.tpe =:= TypeRepr.of[Nothing]) )
     )

  def buildAwaitValueDiscardExpr(using Quotes)(discardTerm: quotes.reflect.Term, p: Expr[?]):Expr[Any] =
      import quotes.reflect._

      def parseDiscardTermType(tpe: TypeRepr): (TypeRepr, TypeRepr) =
        tpe match
           case AppliedType(base, targs) =>
                  base match
                    case TypeRef(sup, "AwaitValueDiscard") =>
                       targs match
                         case List(tf, tt) => (tf, tt)
                         case _ =>
                             val msg = s"Expected that AwaitValueDiscard have 2 type paraleters, but we have $targs"
                             throw MacroError(msg, discardTerm.asExpr)
                    case _ =>
                       val msg = s"Reference to AwaitValueDiscard expected"
                       throw MacroError(msg, discardTerm.asExpr)
           case _ =>
                  val msg = s"Can't parse AwaitValueDiscard type, tpe=${tpe}"
                  throw MacroError(msg, discardTerm.asExpr)

      discardTerm.tpe.asType match
        case '[AwaitValueDiscard[F,tt]] =>
           val refP = p.asExprOf[F[tt]]
           '{  await[F,tt,F]($refP)(using ${cpsCtx.monad}, ${cpsCtx.monadContext})  }
        //bug in dotty. TODO: submit
        //case '[AwaitValueDiscard[[xt]=>>ft,tt]] =>
        //   ???
        case _ => 
           val (ftr, ttr) = parseDiscardTermType(discardTerm.tpe)
           val ftmt = TypeRepr.of[CpsMonad].appliedTo(ftr)
           Implicits.search(ftmt) match
              case monadSuccess: ImplicitSearchSuccess =>
                val ftm = monadSuccess.tree
                Apply(    
                     Apply(
                       TypeApply(Ref(Symbol.requiredMethod("cps.await")), 
                          List(Inferred(ftr),Inferred(ttr),Inferred(ftr))),
                       List(p.asTerm)
                     ),
                     List(ftm, cpsCtx.monadContext.asTerm)
                ).asExpr
              case monadFailure: ImplicitSearchFailure =>
                throw MacroError(s"Can't find appropriative monad for ${discardTerm.show}, ${monadFailure.explanation}  : ", p)
           



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
