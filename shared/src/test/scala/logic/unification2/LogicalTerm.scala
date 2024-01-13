package logic.unification2

import logic.CpsLogicMonad

sealed trait LogicalExpression


sealed trait LogicalTerm extends LogicalExpression {

   type Type
   def symbol: Unifiable[Type]
  
}

object LogicalTerm {

  type Aux[T] = LogicalTerm { type Type = T }

}

sealed trait TypedLogicalTerm[T:Unifiable] extends LogicalTerm {

  type Type = T

  def symbol = summon[Unifiable[T]]

}

case class LogicalVariable[T:Unifiable](id:Long, name: String) extends TypedLogicalTerm[T] 


case class LogicalConstant[T:Unifiable](value: T) extends TypedLogicalTerm[T]


case class LogicalFunctionalTerm[T:Unifiable](args: IndexedSeq[LogicalTerm]) extends TypedLogicalTerm[T] {

  
  
}






