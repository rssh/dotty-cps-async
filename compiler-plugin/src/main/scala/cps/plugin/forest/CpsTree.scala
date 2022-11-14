package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Constants.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import core.Symbols.*
import ast.tpd.*

import cps.plugin.*

/**
 * CpsTree -- transfomed element
 **/
sealed trait CpsTree {
  def tctx: TransformationContext
  
  /**
  * origin term, used for diagnostics.
  **/
  def origin: Tree

  /**
  * owner of origin
  **/
  def originOwner: Symbol

  /**
   * is this is a sync-tree ?
   **/
  def unpure(using Context): Option[Tree] 

  def transformed(using Context): Tree

  def transformedType(using Context): Type =
    CpsTransformHelper.cpsTransformedType(originType, tctx.monadType)
  
  /**
   * is  (can't be reprsesented as pure(x))
   **/
  def isAsync: Boolean 

  def isOriginEqSync(using Context): Boolean =
    unpure match
      case None => false
      case Some(unpureTerm) => (unpureTerm eq origin)

  def originType: Type = origin.tpe

  /**
   * let we have block {A; B}
   * cps({A;B}) = cps(A).appendInBlock(cps(B))
   **/
  def appendInBlock(next: CpsTree): CpsTree

  /**
   *@returns a copy of cpstree with origin set to origin.
   **/
  def withOrigin(term:Tree):CpsTree 



}

object CpsTree {

  def unchangedPure(tctx: TransformationContext, origin: Tree, owner: Symbol): PureCpsTree =
     PureCpsTree(tctx, origin, owner, origin)


  def pure(tctx: TransformationContext, origin: Tree, owner: Symbol, changed: Tree): PureCpsTree =
     PureCpsTree(tctx, origin, owner, changed)

  def impure(tctx: TransformationContext, origin: Tree, owner: Symbol, impure: Tree): CpsTree =
     AsyncTermCpsTree(tctx, origin, owner, impure)

  def unit(tctx: TransformationContext, owner: Symbol): SyncCpsTree =
     UnitCpsTree(tctx, owner)

}

sealed trait SyncCpsTree extends CpsTree {

  def isAsync: Boolean = false

  def getUnpure(using Context): Tree

  def unpure(using Context): Option[Tree] = Some(getUnpure)

  override def transformed(using Context): Tree = {
    val pureName = "pure".toTermName
    // TODO: setPos
    Apply(
      TypeApply(
        Select(tctx.cpsMonadRef,pureName),
        List(TypeTree(originType.widen))
      ),
      List(getUnpure)
    )
     .withSpan(origin.span)
     .withType(transformedType)
  }


}


case class PureCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  override val term: Tree
) extends SyncCpsTree {

  override def unpure(using Context) = Some(term) 

  override def getUnpure(using Context) = term 

  def appendInBlock(next: CpsTree): CpsTree =

}

case class AsyncTermCpsTree(
  tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val transformedTree: Tree
) extends CpsTree {

  def isAsync: Boolean = true

  override def transformed(using Context): Tree =
    transformedTree

}


case class AwaitSyncCpsTree(
  override val tctx: TransformationContext,
  override val origin: Apply,
  override val originOwner: Symbol,
           val awaitArg: Tree
) extends CpsTree {

   def isAsync: Boolean = false

   override def transformed(using Context): Tree = awaitArg
   override def transformedType(using Context) = awaitArg.tpe

}

case class MapCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val mapSource: CpsTree,
  val mapFun: MapCpsTreeArgument  //  lambda function
) extends CpsTree {

  def isAsync: Boolean =
        mapSource.isAsync

  override def transformed(using Context): Tree = {
    mapSource.unpure match 
      case None =>
        val mapName = "map".toTermName
        Apply(
          Apply(
            TypeApply(
              Select(tctx.cpsMonadRef, mapName),
              List(TypeTree(mapSource.originType.widen), TypeTree(originType.widen))
            ),
            List(mapSource.transformed)
          ),
          List(mapFun.makeLambda(originOwner))
        ).withSpan(origin.span)
      case Some(unpureTerm) =>
        if (!(unpureTerm eq origin)) {
          Apply(mapFun.makeLambda(originOwner) ,List(unpureTerm))
              .withSpan(origin.span)
        } else {
          origin
        }
  }

}


case class MapCpsTreeArgument(
    param: ValDef,
    body:  CpsTree
)  {

  def makeLambda(owner: Symbol)(using Context): Block = {
    body.unpure match
      case None =>
        throw CpsTransformException("attempt use MapCpsTree with async argumemt. use FlatMapCpsTree instead",body.origin.srcPos)
      case Some(syncBody) =>
        TransformUtil.makeLambda(List(param), body.originType.widen, owner, syncBody, body.originOwner)
  }  
  
}

case class FlatMapCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol,
  val flatMapSource: CpsTree,
  val flatMapFun: FlatMapCpsTreeArgument
) extends CpsTree {

  def isAsync: Boolean = true

  override def transformed(using Context): Tree =
    val flatMapName = "flatMap".toTermName
    Apply(
      Apply(
        TypeApply(
          Select(tctx.cpsMonadRef, flatMapName),
          List(TypeTree(flatMapSource.originType), TypeTree(originType))
        ),
        List(flatMapSource.transformed)
      ),
      List(flatMapFun.makeLambda(originOwner))
    ).withSpan(origin.span)

}

case class FlatMapCpsTreeArgument(
   param: ValDef, 
   body: CpsTree
) {

   def makeLambda(owner: Symbol)(using Context): Block = {
    val transformedBody = body.transformed(using summon[Context].withOwner(body.originOwner))
    TransformUtil.makeLambda(List(param),body.transformedType,owner,transformedBody, body.originOwner)
  }


}


/**
 *lambda without type-params and one argument list.
 **/
case class LambdaCpsTree(
  override val tctx: TransformationContext,
  override val origin: Block,
  override val originOwner: Symbol,
  val originDefDef: DefDef,
  val cpsBody: CpsTree
)  extends CpsTree {

  if (originDefDef.paramss.length != 1) {
    throw new  CpsTransformException("Lambda function can have only one parameter list",origin.srcPos)
  }

  def isAsync: Boolean = cpsBody.isAsync

  override def transformed(using Context): Tree = {
    val tpe = createShiftedType()
    val meth = Symbols.newAnonFun(summon[Context].owner,tpe)
    // here cpsBody is received in other context
    // .  TODO:  check ownitu in cpsBody.transformed
    Closure(meth, tss => TransformUtil.substParams(cpsBody.transformed, originParams, tss.head).changeOwner(cpsBody.originOwner, meth))
  }

  private def originParams(using Context) = originDefDef.termParamss.head

  private def createShiftedType()(using Context): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe)
    MethodType(paramNames)(
      x => paramTypes,
      x => CpsTransformHelper.cpsTransformedType(cpsBody.transformedType, tctx.monadType)
    )    
  }
}


/**
 * one Unit 
 **/
case class UnitCpsTree(override val tctx: TransformationContext, override val originOwner: Symbol) extends SyncCpsTree {

    override def origin: Tree = EmptyTree

    override def getUnpure(using Context): Tree =
        Literal(Constant(()))

}

case class BlockBoundsCpsTree(internal:CpsTree) extends CpsTree {
  
}