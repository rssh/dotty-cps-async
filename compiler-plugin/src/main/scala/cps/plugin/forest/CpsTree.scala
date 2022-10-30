package cps.plugin.forest

import dotty.tools.dotc.*
import core.*
import core.Contexts.*
import core.Types.*
import core.Decorators.*
import ast.tpd.*

import cps.plugin.*

sealed trait CpsTree {
  def monadType: Type
  
  def cpsMonadRef: Tree
  
  def unchangedOrigin: Tree

  def isOriginChanged: Boolean = false

  def changedOrigin(using Context): Tree =
    unchangedOrigin

  def transformed(using Context): Tree

  def transformedType(using Context): Type =
    CpsTransformHelper.cpsTransformedType(originType, monadType)
  
  def isAsync: Boolean 

  def originType: Type = unchangedOrigin.tpe

} 


case class PureCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Tree,
) extends CpsTree {

  def isAsync: Boolean = false

  override def transformed(using Context): Tree = {
    val pureName = "pure".toTermName
    // TODO: setPos
    Apply(
      TypeApply(
        Select(cpsMonadRef,pureName),
        List(TypeTree(originType.widen))
      ),
      List(changedOrigin)
    )
     .withSpan(unchangedOrigin.span)
     .withType(transformedType)
  }

}

case class AsyncTermCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Tree,
  val transformedTree: Tree
) extends CpsTree {

  def isAsync: Boolean = true

  override def transformed(using Context): Tree =
    transformedTree

}


case class AwaitSyncCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Apply,
           val awaitArg: Tree
) extends CpsTree {

   def isAsync: Boolean = false

   override def transformed(using Context): Tree = awaitArg
   override def transformedType(using Context) = awaitArg.tpe

}

case class MapCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Tree,
  val mapSource: CpsTree,
  val mapFun: Tree  //  lambda function
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
        List(mapFun)
      ).withSpan(unchangedOrigin.span)
    } else if (!isOriginChanged) {
      Apply(mapFun,List(mapSource.changedOrigin))
            .withSpan(unchangedOrigin.span)
    } else {
      unchangedOrigin
    }
  }

}

case class FlatMapCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Tree,
  val flatMapSource: CpsTree,
  val flatMapFun: Tree
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

/*
case class LambdaCpsTree(
  override val monadType: Type,
  override val cpsMonadRef: Tree,
  override val unchangedOrigin: Block,
  val defDef: DefDef,
  val transformedBody: CpsTree
)  {

  

}
*/
