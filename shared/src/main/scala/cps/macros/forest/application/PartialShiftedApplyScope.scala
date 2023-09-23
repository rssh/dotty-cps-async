package cps.macros.forest.application

import cps.*
import cps.macros.forest.*

/*
sealed trait ApplicationShiftType

object ApplicationShiftType {

  case object CPS_ONLY extends ApplicationShiftType
  case class CPS_AWAIT[F](runtimeAwait: Expr[CpsRuntimeAwait[F]]) extends ApplicationShiftType

}

 */

enum ApplicationShiftType:
  case CPS_ONLY
  case CPS_AWAIT

trait  PartialShiftedApplyScope[F[_], CT, CC<:CpsMonadContext[F]]:

  thisTreeTransform: TreeTransformScope[F,CT, CC] =>

  import qctx.reflect._


  /**
  * Application with one list of params.
  * 
  */
  case class PartialShiftedApply(
    shiftType: ApplicationShiftType,

    /**
     * function, which will be applied
     * argument is runtimeAwait, which is needed when type of shift is CPS_AWAIT.
     * when type of shift is CPS_ONLY, runtimeAwait is not used (and can be emoty term)
     */
    shiftedDelayed: Term => Term,
                                ):

    def withTailArgs(argTails: List[Seq[ApplyArgRecord]], withAsync: Boolean): Term => Term =
      runtimeAwait => {
        val shiftedTails = shiftType match
          case ApplicationShiftType.CPS_ONLY =>
            argTails.map(_.map(_.shift().identArg(withAsync)).toList)
          case ApplicationShiftType.CPS_AWAIT =>
            argTails.map(_.map(_.withRuntimeAwait(runtimeAwait).identArg(withAsync)).toList)
        shiftedDelayed(runtimeAwait).appliedToArgss(shiftedTails)
      }

