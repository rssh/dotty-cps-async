package cps

import scala.compiletime.*
import scala.util.NotGiven

trait CpsMonadContext[F[_]] {

  /**
   * adopt monadic value in await to current context.
   **/
  def adoptAwait[A](fa:F[A]):F[A]
 
}

trait CpsContextType[F[_]] {
  type Context <: CpsMonadContext[F]
}

object CpsContextType {

  type Resolve[X <: CpsMonad[_]] = X match
    case CpsMonadInstanceContext[f] => CpsMonadInstanceContext[f]
    case CpsContextMonad[f,c] => c


  given instanceContextType[F[_]](using m:CpsMonadInstanceContext[F]): CpsContextType[F] =
    new CpsContextType[F] {
       type Context = m.type
    }

  given suppliedContextType[F[_],C <: CpsMonadContext[F]](using CpsContextMonad[F,C]): CpsContextType[F] with {
      type Context = C
  }


}

/*
trait CpsResolveContextType[F[_]] {
   type Context
}

object CpsResolveContextType {

  //transparent inline given existingContextType[F[_],C](using ct: CpsContextType.Aux[F,C]): CpsResolveContextType[F] =
  //  new CpsResolveContextType[F]  {
  //    type Context = C
  //  }

  given instanceContextType[F[_]](using m:CpsMonadInstanceContext[F]): CpsResolveContextType[F] with {
       type Context = m.type
  }

  given suppliedContextType[F[_],C <: CpsMonadContext[F]](using CpsContextMonad[F,C]): CpsResolveContextType[F] with {
      type Context = C
  }


  given notFoundContextType[F[_]](using ct: NotGiven[CpsContextType[F]]): CpsResolveContextType[F] with {
    type Context = CpsMonadContext[F]
  }

 
}
*/

object CpsMonadContext {

   /*
   type FindContext[X] = X match
     case CpsMonadInstanceContext[f] => f
     case CpsContextMonad[f,c] => c
     case _ => CpsMonadContext[F] 
   */

   transparent inline def resolve[F[_]]: CpsContextType[F] = summonFrom {
      case a: CpsContextType[F] => a
      case _ => new CpsContextType[F] {
                   type Context = CpsMonadContext[F] 
                } 
   }

}


trait CpsMonadInstanceContext[F[_]] extends CpsMonad[F] with CpsMonadContext[F] {

  override type Context = CpsMonadInstanceContext[F]

  /**
  * run with this instance
  **/
  def apply[T](op: Context => F[T]): F[T] =
    op(this.asInstanceOf[Context])

  /**
  *@return fa
  **/
  def adoptAwait[A](fa:F[A]):F[A] = fa

}

trait CpsContextMonad[F[_],Ctx <: CpsMonadContext[F]]  extends CpsMonad[F] {

  override type Context = Ctx

  def applyContext[T](op: Ctx =>F[T]): F[T]  

  def apply[T](op: Context => F[T]): F[T] =
    applyContext(c => op(c.asInstanceOf[Context]))
  
}
