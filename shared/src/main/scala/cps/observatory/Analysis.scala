package cps.observatory

import cps.*
import scala.quoted.*

trait Analysis
{

  var enabled: Boolean = false

  /**
   * Called for each element of tree batched with other analysis in traversor.
   *
   * During prelimnary analysis stge, preliminaryTreeVisit will called
   * for each tree and each subtrees.
   *
   *@return - true if we want to process subtrees of this tree, otherwise - false.
   **/
  def visitStart[F[_]:Type](using qctx: Quotes)(
                   tree: qctx.reflect.Tree, ctx: ObservationContext[F])(owner: qctx.reflect.Symbol) : Unit = { }

  def visitDone[F[_]:Type](using qctx: Quotes)( 
                   tree: qctx.reflect.Tree, ctx: ObservationContext[F]): Unit = { }

  /**
   * run after all preliminary anlayses has been runned.
   *
   **/
  def cpsVisit[F[_],T](using qctx: Quotes)(tree: qctx.reflect.Tree, ctx: TransformationContext[F,T]): Unit = { }

}
