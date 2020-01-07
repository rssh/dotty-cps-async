// CPS Transform for tasty apply
// (C) Ruslan Shevchenko <ruslan@shevchenko.kiev.ua>, 2019
package cps.forest

import scala.quoted._
import scala.quoted.matching._

import cps._


class ApplyTransform[F[_]:Type,T:Type](cpsCtx: TransformationContext[F,T])

  import cpsCtx._

  // case Apply(fun,args) 
  def run(given qctx: QuoteContext)(fun: qctx.tasty.Term, args: List[qctx.tasty.Term]): CpsExprResult[F,T] =
     import qctx.tasty.{_, given}
     println(s"!!! apply detected : ${fun} ${args}")
     // check, that rFun is a method-call, in such case, we can't transform tree to expr
     //  without eta-expansion.  So, wait
     //val etaExpanded = fun match {
     //  case Select(qual,name) => fun.etaExpand
     //  case _ => fun
     //}
     //if (!(etaExpanded eq fun)) {
     //  println(s"before eta-expand: ${fun.show}")
     //  println(s"after eta-expand: ${etaExpanded.show}")
     //} 
     fun.tpe.widen match {
       case _ : MethodType | PolyType  =>
          // term should be eta-expanded before,
         fun match 
           case Select(qual,name)  => 
             println("Select qual")
             val qualExpr = qual.seal
             qualExpr match 
                 case '{ $q: $qt } =>
                    val cpsObj = callRootTransform(q)
                    adoptArgs(cpsObj,(x:Term) => Select(x,fun.symbol) ,args) 
           case Ident(sym) =>
             val expanded = fun.etaExpand
             val expandedExpr = expanded.seal
             expandedExpr match {
                case '{ $fe: $fte } =>
                  val cpsFun = callRootTransform(fe)
                  adoptArgs(cpsFun,(x:Term) => x ,args) 
             }
       case _ =>
           val funExpr = fun.seal
           funExpr match 
              case '{ $fun: $funType } =>
                 val rFun = callRootTransform(fun)
                 adoptArgs(rFun,  (x:Term) => x, args)
     }

  
  def adoptArgs[S:Type](given qctx: QuoteContext)(cpsObjResult: CpsExprResult[F,S], 
                  objFun: qctx.tasty.Term => qctx.tasty.Term,
                  args:List[qctx.tasty.Term]):CpsExprResult[F,T] = {
        //  For now we don't adopt args, but just leave ones unchanged.
        //  TODO: implement  
        import qctx.tasty.{_, given}
        val nextCpsBuilder = if (cpsObjResult.haveAwait) {
              val ftype = patternCode match {
                            case '{ $x:$ft } => ft
                            case _ => qctx.error("Can't retrieve type for ${patternCode.show}")
                                      throw new IllegalStateException("msg")
                          }
              new CpsChunkBuilder[F,T](asyncMonad) {

                def createApply(x:Term):Expr[T] =
                     Apply(objFun(x),args).seal.asInstanceOf[Expr[T]]

                override def create(): CpsChunk[F,T] = 
                   val fc = '{ 
                      ${asyncMonad}.map(
                           ${cpsObjResult.transformed}
                         )( x => ${createApply('x.unseal)} )
                   }
                   fromFExpr(fc)

                override def append[A:quoted.Type](e:CpsChunk[F,A]): CpsChunk[F,A] = 
                    CpsChunk(Seq(),'{
                        ${asyncMonad}.flatMap(
                            ${asyncMonad}.map(${cpsObjResult.transformed}
                                             )( x => ${createApply('x.unseal)} )
                          )(_ => ${e.toExpr})
                       })

              }
        } else {
              CpsChunkBuilder.sync(patternCode,asyncMonad)
        }
        CpsExprResult(patternCode,nextCpsBuilder,patternType,cpsObjResult.haveAwait)
  }
     
  def callRootTransform[T:Type](q:Expr[T])(
                                   given qctx: QuoteContext):CpsExprResult[F,T] = 
         Async.rootTransform[F,T](q, asyncMonad, false)
     


