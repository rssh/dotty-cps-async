package cps.macros.observatory

import scala.quoted.*
import cps.*
import cps.macros.*

trait ObservationContextQuoteScope:

  this: ObservatoryFullQuoteScope =>

  trait ObservationContext[F[_]:Type]:

     import quotes.reflect.*

     def scheduleChildrenVisit(tree: Tree, analysis: Analysis)(owner: Symbol): Unit =
         val acc = new TreeAccumulator[Unit]() {
            override def foldTree(u:Unit, tree:Tree)(owner: Symbol) =
            scheduleVisit(tree, analysis)(owner)
         }
         acc.foldOverTree((),tree)(owner)

     def scheduleVisit(tree: Tree, analysis: Analysis)(owner: Symbol): Unit

     def fTypeRepr: TypeRepr = TypeRepr.of[F]

     def flags: AsyncMacroFlags 


