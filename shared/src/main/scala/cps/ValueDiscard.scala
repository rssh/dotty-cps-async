package cps


trait ValueDiscard[T]:

  def apply(value:T): Unit


object ValueDiscard:

  class EmptyValueDiscard[T] extends ValueDiscard[T]:

    inline override def apply(value: T): Unit = {}

  transparent inline given intValueDiscard as ValueDiscard[Int] = EmptyValueDiscard[Int]
  transparent inline given longValueDiscard as ValueDiscard[Long] = EmptyValueDiscard[Long]
  transparent inline given booleanValueDiscard as ValueDiscard[Boolean] = EmptyValueDiscard[Boolean]
  transparent inline given stringValueDiscard as ValueDiscard[String] = EmptyValueDiscard[String]
    



