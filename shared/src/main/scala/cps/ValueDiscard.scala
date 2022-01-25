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

  class WarnTag {}

  class CustomTag {}


/**
 * Marker interface for forcing monad evaluation before discard.
 * Useful for pure effect monads. 
 * `AwaitValueDiscard[F,T].apply(ft)` is transformed to `await(ft)` during evaluation of async macro.
 **/
class AwaitValueDiscard[F[_]:CpsMonad,T] extends ValueDiscard[F[T]]:

  type FT = F[T]
  type TT = T



  /**
   * transformed to `await(value)`.
   **/
  @compileTimeOnly("AwaitValueDiscard should not be used directly")
  transparent inline override def apply(value: F[T]): Unit = ???


/**
 * marker object for value discarding.
 *  When this object is imported into current scope,
 *  then discarding values inside async block is translated
 *  to summon[ValueDiscard[T]].apply()
 **/
given customValueDiscard: ValueDiscard.CustomTag with {}


/**
 * marker object for enabling warning about discarding non-primitve values without custom discard.
 **/
given warnValueDiscard: ValueDiscard.WarnTag with {}

