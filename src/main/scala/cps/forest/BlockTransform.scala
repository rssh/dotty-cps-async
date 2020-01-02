// CPS Transform for tasty block
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class BlockTransform[F[_]:Type, T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Block(prevs,last) 
  def run(given qctx: QuoteContext)(prevs: List[qctx.tasty.Statement], last: qctx.tasty.Term): CpsExprResult[F,T] =
     val tType = implicitly[Type[T]]
     import qctx.tasty.{_, given}
     println("!!! block detected:1:")
     val rPrevs = prevs.map{ 
          case d: Definition =>
            d match {
              case v@ValDef(vName,vtt,optRhs) =>
                println("!!!ValDef in block")
                optRhs match {
                     case Some(rhs) =>
                         val patternExpr = Block(List(v),Literal(Constant(()))).seal
                         Async.rootTransform[F,Unit](patternExpr.asInstanceOf[Expr[Unit]],asyncMonad) 
                     case None =>
                         val msg = "ValDef without right part: $v"
                         qctx.error(msg,patternCode)
                         throw new IllegalStateException(msg)
                }
              case _ =>
                qctx.error("definition is not supported inside block")
                throw new IllegalStateException("definition inside block")
            } 
          case t: Term =>
            t.seal match 
                case '{ $p:$tp } =>
                        callRootTransform(p,tp)
                case other =>
                        printf(other.show)
                        val msg = s"can't handle statement in block: $other"
                        qctx.error(msg)
                        throw new IllegalStateException(msg)
     }
     val rLast = Async.rootTransform[F,T](last.seal.asInstanceOf[Expr[T]],asyncMonad)
     val lastChunk = rLast.cpsBuild.create()
     val blockResult = rPrevs.foldRight(lastChunk)((e,s) => e.cpsBuild.append(s))
     val haveAwait = rLast.haveAwait || rPrevs.exists(_.haveAwait)
     val cpsBuild = new CpsChunkBuilder[F,T] {
            override def create() = CpsChunk(Seq(),blockResult.toExpr)
            override def append[A:quoted.Type](e: CpsChunk[F,A]) = 
                        if (!haveAwait) 
                          e.insertPrev(patternCode)
                        else
                          CpsChunk(Seq(), 
                           '{ ${asyncMonad}.flatMap(${blockResult.toExpr})( _ => ${e.toExpr} )   }
                          )
     }                                 
     CpsExprResult[F,T](patternCode,cpsBuild,patternType,haveAwait)
  
  def callRootTransform[P:Type](expr:Expr[P],pType:Type[P])(given QuoteContext): CpsExprResult[F,P] =
                        Async.rootTransform[F,P](expr,asyncMonad)


