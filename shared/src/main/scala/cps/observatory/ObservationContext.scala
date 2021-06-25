package cps.observatory

import scala.quoted.*
import cps.*


trait ObservationContext[F[_]:Type]:

   def scheduleChildrenVisit(using qctx: Quotes)(
                       tree: qctx.reflect.Tree, analysis: Analysis)(owner: qctx.reflect.Symbol): Unit =
     import qctx.reflect.*
     val acc = new TreeAccumulator[Unit]() {
        override def foldTree(u:Unit, tree:Tree)(owner: Symbol) =
           scheduleVisit(tree, analysis)(owner)
     }
     acc.foldOverTree((),tree)(owner)

   def scheduleVisit(using qctx: Quotes)(tree: qctx.reflect.Tree, analysis: Analysis)(owner: qctx.reflect.Symbol): Unit

   def fTypeRepr(using qctx: Quotes): qctx.reflect.TypeRepr = 
      qctx.reflect.TypeRepr.of[F]


