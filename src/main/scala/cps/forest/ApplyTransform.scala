// CPS Transform for tasty apply
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._



class ApplyTransform[F[_]:Type, T:Type](f: Expr[T], dm:Expr[AsyncMonad[F]])


  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(fun: qctx.tasty.Term, args: List[qctx.tasty.Term]): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     println(s"!!! apply detected : ${fun} ${args}")
     // check, that rFun is a method-call, in such case, we can't transform tree to expr
     //  without eta-expansion.  So, wait
     fun match {
       case Select(qual,name) => 
         println("Select qual")
         val qualExpr = qual.seal
         qualExpr match 
            case '{ $q: $qt } =>
              val cpsObj = callRootTransform(q)
              adoptArgs(cpsObj,(x:Term) => Select(x,fun.symbol) ,args) 
       case _ =>
         val funExpr = fun.seal
         funExpr match 
            case '{ $fun: $funType } =>
              val rFun = callRootTransform(fun)
              println(s"!!! rFun= : ${rFun}")
              adoptArgs(rFun,  (x:Term) => x, args)
     }
  
  def adoptArgs[S:Type](given qctx: QuoteContext)(cpsObjResult: CpsExprResult[F,S], 
                  objFun: qctx.tasty.Term => qctx.tasty.Term,
                  args:List[qctx.tasty.Term]):CpsExprResult[F,T] = {
        //  For now we don't adopt args, but just leave ones unchanged.
        //  TODO: implement  
        import qctx.tasty.{_, given}
        val nextCpsBuilder = if (cpsObjResult.haveAwait) {
              val ftype = f match {
                            case '{ $x:$ft } => ft
                            case _ => qctx.error("Can't retrieve type for ${f.show}")
                                      throw new IllegalStateException("msg")
                          }
              new CpsChunkBuilder[F,T] {

                def createApply(x:Term):Expr[T] =
                     Apply(objFun(x),args).seal.asInstanceOf[Expr[T]]

                val objChunk = cpsObjResult.cpsBuild.create()

                override def create(): CpsChunk[F,T] = 
                   val fc = '{ 
                      ${dm}.map(${objChunk.toExpr})( x => ${createApply('x.unseal)} )
                   }
                   fromFExpr(fc)

                override def append[A:quoted.Type](e:CpsChunk[F,A]): CpsChunk[F,A] = 
                    CpsChunk(Seq(),'{
                        ${dm}.flatMap(
                            ${dm}.map(${objChunk.toExpr})( x => ${createApply('x.unseal)} )
                          )(_ => ${e.toExpr})
                       })

              }
        } else {
              new CpsChunkBuilder[F,T] {
                  override def create(): CpsChunk[F,T] = 
                        fromFExpr('{ ${dm}.pure(${f}) })
                   override def append[A:quoted.Type](e:CpsChunk[F,A]):CpsChunk[F,A] =
                        e.insertPrev(f)
              }
        }
        CpsExprResult(f,nextCpsBuilder,summon[quoted.Type[T]],cpsObjResult.haveAwait)
  }
     
  def callRootTransform[T:Type](q:Expr[T])(
                                   given qctx: QuoteContext):CpsExprResult[F,T] = 
         Async.rootTransform[F,T](q, dm)
     


