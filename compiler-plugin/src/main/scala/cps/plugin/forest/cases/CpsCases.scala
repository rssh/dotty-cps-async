package cps.plugin.forest.cases

import dotty.tools.dotc.*
import ast.tpd.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*

import cps.plugin.*
import cps.plugin.forest.*


class CpsCases(val cases: List[CpsCaseDef]) {


  def collectAsyncKind(using Context, CpsTopLevelContext): AsyncKind =
    cases.foldLeft(None:Option[AsyncKind]) { (acc, c) =>
      acc match
        case None => Some(c.cpsBody.asyncKind)
        case Some(x) => x.unify(c.cpsBody.asyncKind) match
          case Right(x) => Some(x)
          case Left(msg) => throw CpsTransformException("Can't unify async shape in case branches for match", c.origin.srcPos)
    } match
      case Some(x) => x
      case None => throw CpsTransformException("Can't unify async shape in case branches for match", cases.head.origin.srcPos)

  def  transformedCaseDefs(targedKind:AsyncKind, targetType: Type, nesting: Int)(using Context, CpsTopLevelContext): List[CaseDef] =
    val retval = cases.map(_.transform(targedKind, targetType, nesting))
    retval

  def  unpureCaseDefs(using Context, CpsTopLevelContext): List[CaseDef] =
    cases.map(c => CaseDef(c.origin.pat,c.origin.guard,c.cpsBody.unpure.get))

  def unchanged(using Context, CpsTopLevelContext): Boolean =
    cases.forall(_.cpsBody.isOriginEqSync)

}

object CpsCases {

  def create(cases: List[CaseDef], owner: Symbol, nesting:Int)(using Context, CpsTopLevelContext): CpsCases =
      val cpsCases = cases.map( c => CpsCaseDef(c,RootTransform(c.body,owner, nesting+1)) )
      CpsCases(cpsCases)


}
