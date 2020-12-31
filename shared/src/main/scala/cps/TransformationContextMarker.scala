package cps

sealed trait TransformationContextMarker 

object TransformationContextMarker {

  case object TopLevel extends TransformationContextMarker
  case object Repeated extends TransformationContextMarker
  case object AssignLeft extends TransformationContextMarker
  case object AssignRight extends TransformationContextMarker
  case object AssignSelect extends TransformationContextMarker
  case class  BlockInside(i:Int) extends TransformationContextMarker
  case object BlockLast extends TransformationContextMarker
  case object ValDefRight extends TransformationContextMarker
  case object IfCond extends TransformationContextMarker
  case object IfTrue extends TransformationContextMarker
  case object IfFalse extends TransformationContextMarker
  case object InlinedBody extends TransformationContextMarker
  case class  InlinedBinding(i:Int) extends TransformationContextMarker
  case object SelectOuter extends TransformationContextMarker
  case object ApplyTypeApplySelect extends TransformationContextMarker
  case object ApplyTypeApply extends TransformationContextMarker
  case object ApplySelect extends TransformationContextMarker
  case object ApplyFun extends TransformationContextMarker
  case class ApplyArg(i:Int) extends TransformationContextMarker
  case object Await extends TransformationContextMarker
  case object Lambda extends TransformationContextMarker
  case object MatchScrutinee extends TransformationContextMarker
  case object MatchCase extends TransformationContextMarker
  case object Select extends TransformationContextMarker
  case object ThrowException extends TransformationContextMarker
  case object TryBody extends TransformationContextMarker
  case class TryCase(i:Int) extends TransformationContextMarker
  case object TryFinally extends TransformationContextMarker
  case object TypeApplyFun extends TransformationContextMarker
  case object Typed extends TransformationContextMarker
  case object WhileCond extends TransformationContextMarker
  case object WhileBody extends TransformationContextMarker
  case object Other extends TransformationContextMarker

}
