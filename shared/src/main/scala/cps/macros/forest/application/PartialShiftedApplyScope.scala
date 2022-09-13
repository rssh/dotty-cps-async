package cps.macros.forest.application

import cps.*
import cps.macros.forest.*

enum ApplicationShiftType:
  case CPS_ONLY, CPS_AWAIT


trait  PartialShiftedApplyScope[F[_], CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._


  /**
  * Application with one list of params.
  * 
  */
  case class PartialShiftedApply(
    shiftType: ApplicationShiftType,
    shifted: Term
  ):

    def withTailArgs(argTails: List[Seq[ApplyArgRecord]], withAsync: Boolean): Term =
        val shiftedTails = argTails.map(_.map(_.shift(shiftType).identArg(withAsync)).toList)
        shifted.appliedToArgss(shiftedTails)

