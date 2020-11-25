package direct

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._


trait DM[F[_]] {

   def pure[T:Type](t:Expr[T]): (ctx:Quotes) ?=> Expr[F[T]]

}

sealed trait CB[T]
case class Done[T](value:T) extends CB[T]

implicit val dmCB: DM[CB] = new DM[CB] {

  def pure[T:Type](t:Expr[T]):(ctx:Quotes) ?=> Expr[CB[T]] =
    '{ Done(${t}) }

}


object A {

  inline def transform[F[_]:DM:Type,T:Type](inline expr: T): F[T] =
   ???
   /*
      error here: https://github.com/lampepfl/dotty/issues/7839
     ${
         A.transformImpl( 'expr )
      }
   */

  def transformImpl[F[_]:DM,T:Type](f: Expr[T])(using Quotes):Expr[F[T]] = 
    f match 
      case Const(t) => summon[DM[F]].pure(f)
      case _ => print(f)
         throw new IllegalStateException("language construction is not supported")


}

class TestBS2:

  def qqq: Int = 2

 /*
  // testcase is submitted to dotty
  @Test def tConstant(): Unit = {
     val c = A.transform[CB,Int](3)
     assert(c == Done(3))
  }
 */
  

