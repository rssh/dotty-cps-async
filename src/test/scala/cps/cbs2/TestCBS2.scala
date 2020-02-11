package direct

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.quoted.matching._


trait DM[F[_]] {

   def pure[T:Type](t:Expr[T]): (ctx:QuoteContext) ?=> Expr[F[T]]

}

sealed trait CB[T]
case class Done[T](value:T) extends CB[T]

implicit val dmCB: DM[CB] = new DM[CB] {

  def pure[T:Type](t:Expr[T]):(ctx:QuoteContext) ?=> Expr[CB[T]] =
    '{ Done(${t}) }

}


object A {

  inline def transform[F[_]:DM,T](expr: =>T): F[T] =
    ${ 
       '{???} //A.transformImpl('expr)
     }

  def transformImpl[F[_]:DM,T:Type](f: Expr[T])(using QuoteContext):Expr[F[T]] = 
    f match 
      case Const(t) => implicitly[DM[F]].pure(f)
      case _ => print(f)
         throw new IllegalStateException("language construction is not supported")


}

class TestBS2:

  @Ignore
  @Test def tConstant(): Unit = {
     val c = A.transform[CB,Int](3)
     assert(c == Done(3))
  }
  

