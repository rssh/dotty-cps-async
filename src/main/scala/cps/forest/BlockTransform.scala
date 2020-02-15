// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T]):

  import cpsCtx._

  // case Block(prevs,last) 
  def run(using qctx: QuoteContext)(prevs: List[qctx.tasty.Statement], last: qctx.tasty.Term): CpsExpr[F,T] =
     val tType = implicitly[Type[T]]
     import qctx.tasty.{_, given _}
     val rPrevs = prevs.zipWithIndex.map{ (p,i) =>
        p match
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                ValDefTransform.fromBlock[F](using qctx)(cpsCtx.copy(exprMarker=exprMarker+i.toString), v)
              case _ =>
                printf(d.show)
                throw MacroError("definition is not supported inside block",patternCode)
            } 
          case t: Term =>
            t.seal match 
                case '{ $p:$tp } =>
                        Async.rootTransform(p, asyncMonad, cpsCtx.exprMarker+i.toString)
                case other =>
                        printf(other.show)
                        throw MacroError(s"can't handle statement in block: $other",t.seal)
     }
     val rLast = Async.rootTransform[F,T](last.seal.asInstanceOf[Expr[T]],asyncMonad,cpsCtx.exprMarker+"B")
     val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
     // wrap yet in one Expr, to 'seal' (not unroll during append in enclosing block).
     CpsExpr.wrap(blockResult)
  


