package cps

import scala.quoted._

trait CpsMonad[F[_]] 

trait ComputationBound[T] 

implicit object ComputationBoundMonad extends CpsMonad[ComputationBound] {
  def pure[T](x:T):ComputationBound[T] = ???
}

inline def async[F[_]](using am:CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object PFHelper {
  def create[X,Y](x:Boolean):PartialFunction[X,Y] = ???
}

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       inline def apply[T](inline expr: T):Unit =
       ${
         Async.transformImpl[F,T]('expr)
        }

  }


  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(using qctx: QuoteContext): Expr[Unit] = 
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
            val fromTypeOrBounds = tparams1.head
            val fromType = fromTypeOrBounds match
                 case bounds: TypeBounds => bounds.low
                 case tp: Type => tp
                 case np: NoPrefix => ???
            val toType = tparams1.tail.head
            val fType = summon[quoted.Type[F]]
            val toWrapped = AppliedType(fType.unseal.tpe,List(toType))
            val helper = '{ cps.PFHelper }.unseal
            val helperSelect = Select.unique(helper,"create")
            val createPF = Apply(
                             TypeApply(helperSelect,List(Inferred(fromType),Inferred(toWrapped))),
                             //List(bodyLambda)
                             List(Literal(Constant(true)))
                           )
            val createPfApply = Apply(Select.unique(createPF,"apply"),List(Literal(Constant(1))))
            Block(List(createPfApply),Literal(Constant(()))).seal.asInstanceOf[Expr[Unit]]


}
