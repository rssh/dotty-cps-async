package cps.features

import cps._


trait customValueDiscard
trait warnValueDiscard

/**
 * marker object for value discarding.
 *  When this object is imported into current scope,
 *  then discarding values inside async block is translated
 *  to summon[ValueDiscard[T]].apply()
 **/
object customValueDiscard:

  given tag as customValueDiscard


object warnValueDiscard:

  given tag as warnValueDiscard
  


trait ValueDiscard[T]:

  def apply(value:T): Unit



object ValueDiscard:

  class EmptyValueDiscard[T] extends ValueDiscard[T]:

    inline override def apply(value: T): Unit = {}


  transparent inline given intValueDiscard as ValueDiscard[Int] = EmptyValueDiscard[Int]
  transparent inline given longValueDiscard as ValueDiscard[Long] = EmptyValueDiscard[Long]
  transparent inline given booleanValueDiscard as ValueDiscard[Boolean] = EmptyValueDiscard[Boolean]
  transparent inline given stringValueDiscard as ValueDiscard[String] = EmptyValueDiscard[String]
    



