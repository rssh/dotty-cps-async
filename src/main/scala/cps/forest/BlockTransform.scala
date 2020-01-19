// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._
import cps.misc._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Block(prevs,last) 
  def run(given qctx: QuoteContext)(prevs: List[qctx.tasty.Statement], last: qctx.tasty.Term): CpsExpr[F,T] =
     val tType = implicitly[Type[T]]
     import qctx.tasty.{_, given}
     println("!!! block detected:1:")
     val rPrevs = prevs.map{ 
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                println(s"!!!ValDef in block : ${v.show}")
                optRhs match {
                     case Some(rhs) =>
                         val patternExpr = Block(List(v),Literal(Constant(()))).seal
                         val patternExprUnit = patternExpr.asInstanceOf[Expr[Unit]]
                         Async.rootTransform[F,Unit](patternExprUnit,asyncMonad,true)
                     case None =>
                         val msg = "ValDef without right part in block: $v"
                         qctx.error(msg,patternCode)
                         throw new IllegalStateException(msg)
                }
              case _ =>
                printf(d.show)
                throw MacroError("definition is not supported inside block",patternCode)
            } 
          case t: Term =>
            t.seal match 
                case '{ $p:$tp } =>
                        callRootTransform(p,tp, true)
                case other =>
                        printf(other.show)
                        throw MacroError(s"can't handle statement in block: $other",t.seal)
     }
     val rLast = Async.rootTransform[F,T](last.seal.asInstanceOf[Expr[T]],asyncMonad,true)
     val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
     // wrap yet in one Expr, to 'seal' (not unroll during append in enclosing block).
     CpsExpr.wrap(blockResult)
  
  def callRootTransform[P:Type](expr:Expr[P],pType:Type[P],inBlock:Boolean)(given QuoteContext): CpsExpr[F,P] =
                        Async.rootTransform[F,P](expr,asyncMonad,inBlock)


