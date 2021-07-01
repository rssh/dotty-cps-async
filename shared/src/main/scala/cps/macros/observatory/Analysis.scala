/**
 * part of dotty-cps-async
 * (C) Ruslan Shevchenko, <ruslan@shevchenko.kiev.ua>, 2020, 2021
 **/
package cps.macros.observatory

import cps.*
import cps.macros.*
import scala.quoted.*

trait AnalysisQuoteScope:

  this: ObservatoryFullQuoteScope  =>

  import quotes.reflect.*

  trait Analysis:

    var enabled: Boolean = false

    /**
     * Called for each element of tree batched with other analysis in traversor.
     *
     * During prelimnary analysis stge, preliminaryTreeVisit will called
     * for each tree and each subtrees.
     *
     *@return - true if we want to process subtrees of this tree, otherwise - false.
     **/
    def visitStart[F[_]:Type](tree: Tree, ctx: ObservationContext[F])(owner: Symbol) : Unit = { }

    /**
     * called for each tree when we leave node.
     **/
    def visitDone[F[_]:Type](tree: Tree, ctx: ObservationContext[F])(owner: Symbol): Unit = { }
                 
     
    /**
     * called after tree traverse, to report errors if any
     */
    def afterTreeTraverse(flags: AsyncMacroFlags): Unit = { }

