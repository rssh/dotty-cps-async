package cps.monads

import cps.*
import scala.annotation.implicitAmbiguous
import scala.collection.*
import scala.util.Try


/**
 * Monad for iterable collection.
 * typical usage:
 * ```
 *    def allpairs[A](l:List[A]):List((A,A)) = reify[List] {
 *        (reflect(l),reflect(l)) 
 *    }
 * ```
 **/
 class IterableCpsMonad[C[x]<:IterableOnce[x]](iterableFactory:IterableFactory[C]) extends CpsThrowMonad[C] with CpsThrowMonadInstanceContext[C] {

    override def pure[A](a:A):C[A] = {
      iterableFactory.apply(a)
    }

    override def map[A,B](fa:C[A])(f: A=>B):C[B] = {
      val builder = iterableFactory.newBuilder[B]
      val it = fa.iterator
      while(it.hasNext) {
        val v = it.next
        builder.addOne(f(v))
      }
      builder.result
    }

    override def flatMap[A,B](fa:C[A])(f: A=> C[B]) = {
      val builder = iterableFactory.newBuilder[B]
      val it = fa.iterator
      while(it.hasNext) {
        val v = it.next
        val fv = f(v)
        val itfv = fv.iterator
        while(itfv.hasNext) {
          builder.addOne(itfv.next)
        }
      }
      builder.result
    }

    def error[A](e: Throwable): C[A] = {
      throw e
    }

} 


inline given iterableCpsMonad[C[x]<:Iterable[x]]: CpsThrowMonad[C] = {
  import cps.macros.misc.CollectionHelper
  val iterableFactory = CollectionHelper.retrieveIterableFactory[C]
  IterableCpsMonad[C](iterableFactory)
}

given [C[x]<:Iterable[x]]: CpsMonadConversion[C,Iterable] with
  override def apply[T](ft:C[T]):Iterable[T] = ft


