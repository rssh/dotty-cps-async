package cps


trait ValueDiscard[T]:

  def apply(value:T): Unit


object ValueDiscard:

  class EmptyValueDiscard[T] extends ValueDiscard[T]:

    inline override def apply(value: T): Unit = {}

  transparent inline given intValueDiscard: ValueDiscard[Int] = EmptyValueDiscard[Int]
  transparent inline given longValueDiscard: ValueDiscard[Long] = EmptyValueDiscard[Long]
  transparent inline given booleanValueDiscard: ValueDiscard[Boolean] = EmptyValueDiscard[Boolean]
  transparent inline given stringValueDiscard: ValueDiscard[String] = EmptyValueDiscard[String]




