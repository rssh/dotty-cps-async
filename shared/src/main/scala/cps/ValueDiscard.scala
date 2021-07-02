package cps

import scala.annotation._

/**
 * When [[cps.customValueDiscard]] is on,
 * value can be discarded only for types `T` for which exists `ValueDiscard[T]`
 *
 * see [chapter in User Guide](https://rssh.github.io/dotty-cps-async/Features.html#custom-value-discard)
 **/
trait ValueDiscard[T]:

  /**
   * called when value is discarded. 
   */
  def apply(value:T): Unit


object ValueDiscard:

  /**
   * Discard when apply do nothing.
   * Usefule for promitive objects, which can be freely discarded.
   */
  class EmptyValueDiscard[T] extends ValueDiscard[T]:

    inline override def apply(value: T): Unit = {}

  transparent inline given intValueDiscard: ValueDiscard[Int] = EmptyValueDiscard[Int]
  transparent inline given longValueDiscard: ValueDiscard[Long] = EmptyValueDiscard[Long]
  transparent inline given booleanValueDiscard: ValueDiscard[Boolean] = EmptyValueDiscard[Boolean]
  transparent inline given stringValueDiscard: ValueDiscard[String] = EmptyValueDiscard[String]


/**
 * Marker interface for forcing monad evaluation before discard.
 * Useful for pure effect monads.
 **/
class AwaitValueDiscard[F[_]:CpsMonad,T] extends ValueDiscard[F[T]]:

  type FT = F[T]
  type TT = T

  @compileTimeOnly("AwaitValueDiscard should not be used directly")
  transparent inline override def apply(value: F[T]): Unit = ???


