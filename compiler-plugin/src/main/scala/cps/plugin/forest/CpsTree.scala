package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import ast.tpd.*

import cps.plugin.*

sealed trait CpsTree {
  def tctx: TransformationContext,
  
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
    CpsTransformHelper.cpsTransformedType(originType, monadType)
  
  def isAsync: Boolean 

  def originType: Type = origin.tpe

} 


case class PureCpsTree(
  override val tctx: TransformationContext,
  override val origin: Tree,
  override val originOwner: Symbol
) extends CpsTree {

  def isAsync: Boolean = false

  override def transformed(using Context): Tree = {
    val pureName = "pure".toTermName
    // TODO: setPos
    Apply(
      TypeApply(
        Select(tctx.cpsMonadRef,pureName),
        List(TypeTree(originType.widen))
      ),
      List(changedOrigin)
    )
     .withSpan(unchangedOrigin.span)
     .withType(transformedType)
  }

  override def unpure = Some(origin) 

  def getUnpure = origin 

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
  override val tctx: TransformationContext
  override val origin: Apply,
  override val originOwner: Symbol,
           val awaitArg: Tree
) extends CpsTree {

   def isAsync: Boolean = false

   override def transformed(using Context): Tree = awaitArg
   override def transformedType(using Context) = awaitArg.tpe

}

case class MapCpsTree(
  override val tctx: TransformationContext
  override val origin: Tree,
  override val originOwner: Symbol,
  val mapSource: CpsTree,
  val mapFun: MapCpsTreeArgument  //  lambda function
) extends CpsTree {

  def isAsync: Boolean =
        mapSource.isAsync

  override def transformed(using Context): Tree = {
    if (mapSource.isAsync) {
      val mapName = "map".toTermName
      //TODO: setPos
      Apply(
        Apply(
          TypeApply(
            Select(cpsMonadRef, mapName),
            List(TypeTree(mapSource.originType.widen), TypeTree(originType.widen))
          ),
          List(mapSource.transformed)
        ),
        List(mapFun.makeLambda)
      ).withSpan(unchangedOrigin.span)
    } else if (!(unpure eq origin)) {
      Apply(mapFun.makeLambda ,List(mapSource.changedOrigin))
            .withSpan(unchangedOrigin.span)
    } else {
      origin
    }
  }

}


case class MapCpeTreeArgument(
    param: ValDef
    body:  PureCpsTree
)  {

  def makeLambda(using Context): Block = {
    val mt = MethodType(List(param.name))(
      x => List(param.tpe),
      x => body.tpe.widen
    )    
    val meth = Symbols.newAnonFun(summon[Context].owner,mt)
    val retval = Closure(meth,tss => {
          TransformUtil.substParams(body.getUnpure,List(param),tss.head).changeOwner(body.originOwner,meth)
                       .withSpan(body.origin.span)
      }
    )
    retval
  }

  
  
}

case class FlatMapCpsTree(
  override val tctx: TransformationContext
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
          Select(cpsMonadRef, flatMapName),
          List(TypeTree(flatMapSource.originType), TypeTree(originType))
        ),
        List(flatMapSource.transformed)
      ),
      List(flatMapFun)
    ).withSpan(unchangedOrigin.span)

}

case class FlatMapCpsTreeArgument(
   param: ValDef,
   body: CpsTree
) {

  def makeLambda(using Context): Block = {
    val mt = MethodType(List(param.name))(
      x => List(param.tpe),
      x => body.tpe.widen
    )    
    val meth = Symbols.newAnonFun(summon[Context].owner,mt)
    val retval = Closure(meth,tss => {
          TransformUtil.substParams(body.transformed,List(param),tss.head).changeOwner(body.originOwner,meth)
                       .withSpan(body.origin.span)
      }
    )
    retval
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

  if (originDefDef.termParamss.length != 1) {
    throw new CpsTransfromException("Lambda function can have only one parameter list")
  }

  def isAsync: Boolean = transformedBody.isAsync

  override def transformed(using Context): Tree = {
    val tpe = createShiftetType()
    val meth = Symbols.newAnonFun(summon[Context].owner,tpe)
    // here cpsBody is received in other context
    // .  TODO:  check ownitu in cpsBody.transformed
    Closure(meth, tss => TransformUtil.substParams(cpsBody.transformed, originParams, tss.head).changeOwner(meth))
  }

  private def originParams = originDefDef.termParamss.head

  private def createShiftetType(): Type = {
    val params = originDefDef.termParamss.head
    val paramNames = params.map(_.name)
    val paramTypes = params.map(_.tpe)
    MethodType(paramNames)(
      x => paramTypes,
      x => CpsHelper.cpsTransformedType(body.tpe.widen, fType)
    )    
  }
}


case class BlockCpsTree(
  revStats:List[CpsTree], last:ExprTree) extends CpsTree

case class ValDefCpsTree()