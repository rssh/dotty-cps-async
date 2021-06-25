package cps.observatory

import java.util.IdentityHashMap
import scala.collection.mutable.ListBuffer
import scala.quoted.*
import cps.*

class Observatory:

   val effectColoring = new AutomaticColoringOfEffects()
   val allAnalysers = Seq(
       effectColoring
   )

   def analyzeTree[F[_]:Type,T:Type](using qctx: Quotes)(tree: qctx.reflect.Tree): Unit =
     import qctx.reflect.*
     val scheduledVisits = new IdentityHashMap[Tree,ListBuffer[Analysis]]
     val batchTraverse = new TreeTraverser() {


       val ctx = new ObservationContext[F] {

           override def scheduleVisit(using q1:Quotes)(tree: q1.reflect.Tree, analysis: Analysis)(
                                               owner: q1.reflect.Symbol): Unit =
               val analyses = scheduledVisits.get(tree)
               if analyses eq null then
                   scheduledVisits.put(tree.asInstanceOf[qctx.reflect.Tree],ListBuffer(analysis))
               else
                   analyses.addOne(analysis)
              
       }

       override def traverseTree(tree: Tree)(owner: Symbol): Unit =
         Option(scheduledVisits.get(tree)).foreach{analysises =>
              for(a <- analysises) 
                 a.visitStart(tree,ctx)
              traverseTreeChildren(tree)(owner)
              for(a <- analysises) 
                 a.visitDone(tree,ctx)
              scheduledVisits.remove(tree)
         }

     }
     scheduledVisits.put(tree,ListBuffer(allAnalysers: _*))
     batchTraverse.traverseTree(tree)(Symbol.spliceOwner)

