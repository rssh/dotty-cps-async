package logic.unification1

sealed trait LogicalExpression {
  def isGround: Boolean
}

sealed trait LogicalTerm extends LogicalExpression {
  def isGround: Boolean
  def ground: Option[LogicalTerm] = if (isGround) Some(this) else None
}

case class LogicalVariableName(name: String, file:String, line:Int, column:Int)

case class LogicalVariable(id:Long, name:LogicalVariableName) extends LogicalTerm {
  override def isGround: Boolean = false
}

sealed trait TypedLogicalTerm[T] extends LogicalTerm

case class LogicalConstant[T:Unifiable](value:T) extends TypedLogicalTerm[T] {
  override def isGround: Boolean = true
}

case class LogicalFunctionSymbol[T](unifiable:Unifiable[T], args: IndexedSeq[LogicalTerm]) extends TypedLogicalTerm[T] {
  override def isGround: Boolean = args.forall(_.isGround)
}

case class CheckedLogicalTerm[T<:Matchable](term: LogicalTerm, cast: Matchable=>Option[T], check: T => Boolean) extends TypedLogicalTerm[T] {
  override def isGround: Boolean = term.isGround
}


sealed trait LogicalValue extends LogicalExpression {
  override def isGround: Boolean = true
}

case object LogicalTrue extends LogicalValue
case object LogicalFalse extends LogicalValue

case class LogicalNot(expr: LogicalExpression) extends LogicalExpression {
  override def isGround: Boolean = expr.isGround
}

case class LogicalAnd(exprs: IndexedSeq[LogicalExpression]) extends LogicalExpression {
  override def isGround: Boolean = exprs.forall(_.isGround)
}

case class LogicalOr(exprs: IndexedSeq[LogicalExpression]) extends LogicalExpression {
  override def isGround: Boolean = exprs.forall(_.isGround)
}

case class HornClause(head: LogicalTerm, body: IndexedSeq[LogicalExpression])

object HornClause {
  
}