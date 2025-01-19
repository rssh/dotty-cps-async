// transform for match
//  (C) Ruslan Shevchenko, 2019-2023
//  , Kiev, Ukraine
package cps.macros.forest

import scala.quoted._

import cps._
import cps.macros._
import cps.macros.common._
import cps.macros.misc._

trait MatchTreeTransform[F[_], CT, CC <: CpsMonadContext[F]]:

  thisScope: TreeTransformScope[F, CT, CC] =>

  import qctx.reflect._

  // case selectTerm @ Select(qualifier,name)
  def runMatch(matchTerm: Match)(owner: Symbol): CpsTree =
    if (cpsCtx.flags.debugLevel >= 15) then
      cpsCtx.log(s"matchTransform: matchTerm.tpe=${matchTerm.tpe}")
      cpsCtx.log(s"matchTransform: matchTerm.tpe.widen=${matchTerm.tpe.widen}")
      cpsCtx.log(s"matchTransform: veryWiden(matchTerm.tpe)=${TransformUtil.veryWiden(matchTerm.tpe)}")
    val widenOtpe = TransformUtil.veryWiden(matchTerm.tpe)
    // val otpe = widenOtpe
    val otpe = matchTerm.tpe
    val scrutinee = matchTerm.scrutinee
    val cpsScrutinee = runRoot(scrutinee)(owner)
    val cpsCases = matchTerm.cases.map(caseDef => runRoot(caseDef.rhs)(owner))
    val asyncCases = cpsCases.exists(_.isAsync)
    val changedCases = cpsCases.exists(_.isChanged)
    if (cpsCtx.flags.debugLevel >= 15) then cpsCtx.log(s"matchTransform: asyncCases=${asyncCases}, changedCases=${changedCases}")
    val nCases = if (asyncCases) {
      (matchTerm.cases zip cpsCases).map { (old, cpstree) =>
        CaseDef.copy(old)(old.pattern, old.guard, cpstree.castOtpe(widenOtpe).transformed)
      }
    } else if (changedCases) {
      (matchTerm.cases zip cpsCases).map { (old, cpstree) =>
        CaseDef.copy(old)(old.pattern, old.guard, cpstree.syncOrigin.get)
      }
    } else {
      matchTerm.cases
    }
    if (!cpsScrutinee.isAsync) then
      if (!asyncCases) then
        if (!changedCases) then CpsTree.pure(owner, matchTerm)
        else CpsTree.pure(owner, Match.copy(matchTerm)(scrutinee, nCases))
      else
        val nTree = Match.copy(matchTerm)(scrutinee, nCases)
        CpsTree.impure(owner, nTree, otpe)
    else if (!asyncCases)
      cpsScrutinee.monadMap(x => Match.copy(matchTerm)(x, nCases), otpe)
    else
      cpsScrutinee.monadFlatMap(x => Match.copy(matchTerm)(x, nCases), otpe)

object MatchTreeTransform:

  def run[F[_]: Type, T: Type, C <: CpsMonadContext[F]: Type](using
      qctx1: Quotes
  )(cpsCtx1: TransformationContext[F, T, C], matchTerm: qctx1.reflect.Match): CpsExpr[F, T] = {

    val tmpFType = summon[Type[F]]
    val tmpCTType = summon[Type[T]]
    val tmpCCType = summon[Type[C]]
    class Bridge(tc: TransformationContext[F, T, C])
        extends TreeTransformScope[F, T, C]
        with TreeTransformScopeInstance[F, T, C](tc) {

      implicit val fType: quoted.Type[F] = tmpFType
      implicit val ctType: quoted.Type[T] = tmpCTType
      implicit val ccType: quoted.Type[C] = tmpCCType

      def bridge(): CpsExpr[F, T] =
        val origin = matchTerm.asInstanceOf[quotes.reflect.Match]
        val owner = quotes.reflect.Symbol.spliceOwner
        runMatch(origin)(owner).toResult[T]

    }
    (new Bridge(cpsCtx1)).bridge()
  }
