package cps

import scala.quoted._

trait CpsMonad[F[_]] 

trait ComputationBound[T] 

implicit object ComputationBoundMonad extends CpsMonad[ComputationBound] 

inline def async[F[_]](using am:CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       inline def apply[T](inline expr: T):Unit =
       ${
         Async.checkPrintTypeImpl[F,T]('expr)
        }

  }


  def checkPrintTypeImpl[F[_]:Type,T:Type](f: Expr[T])(using qctx: QuoteContext): Expr[Unit] = 
    import qctx.tasty.{_,given _}

    def uninline(t:Term):Term =
      t match
        case Inlined(_,_,x) => uninline(x)
        case _ => t

    val fu = uninline(f.unseal)
    fu match 
              case Block(_,Apply(TypeApply(Select(q,n),tparams),List(param))) =>
                   param.tpe match
                      case AppliedType(tp,tparams1) =>
                        val fType = summon[quoted.Type[F]]
                        val ptp = tparams1.tail.head
                        val ptpTree = Inferred(AppliedType(fType.unseal.tpe,List(ptp)))
                        println("res:"+ptpTree.show)
                      case None => 
                        println(s"tpe=${param.tpe} is not AppliedType")
                   '{ () }
              case _ => ???


}
