package logic.unification1

trait Bindings {

  def  get(logicalVariable: LogicalVariable): Option[LogicalTerm]

  def  updated(logicalVariable: LogicalVariable, term: LogicalTerm): Bindings

  def  bind[R[_],A,D](variable: LogicalVariable, a:A)(using Unifiable[A], LogicEngineInstanceData[D]): R[Bindings]

  def  bindM[R[_],A,D](variable: LogicalVariable, a:R[A])(using Unifiable[A], LogicEngineInstanceData[D]): R[Bindings]

  def  newVariable(): LogicalVariable

}
