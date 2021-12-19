package cps.macros.observatory

import java.util.IdentityHashMap
import scala.collection.mutable.ListBuffer
import scala.quoted.*
import cps.*
import cps.macros.*

trait ObservatoryFullQuoteScope extends ObservatoryQuoteScope
                                   with ObservationContextQuoteScope
                                   with AnalysisQuoteScope
                                   with AutomaticColoringOfEffectsQuoteScope


trait ObservatoryQuoteScope:

   this:  ObservatoryFullQuoteScope =>

   val qctx: Quotes
   given Quotes = qctx

   import quotes.reflect.*

   class Observatory(tree: Tree, asyncMacroFlags: AsyncMacroFlags):

     val effectColoring = new AutomaticColoringOfEffects()
     val allAnalysers = Seq(
       effectColoring
     )

     def analyzeTree[F[_]:Type]: Unit =
       val scheduledVisits = new IdentityHashMap[Tree,ListBuffer[Analysis]]
       val batchTraverse = new TreeTraverser() {

         val ctx = new ObservationContext[F] {

           override def scheduleVisit(tree: Tree, analysis: Analysis)(
                                               owner: Symbol): Unit =
               val analyses = scheduledVisits.get(tree)
               if analyses == null then
                   scheduledVisits.put(tree,ListBuffer(analysis))
               else
                   analyses.nn.addOne(analysis)


           override def flags: AsyncMacroFlags = asyncMacroFlags
              
         }

         override def traverseTree(tree: Tree)(owner: Symbol): Unit =
           val toProcess = Option(scheduledVisits.get(tree))
           toProcess.foreach{analysises =>
              for(a <- analysises.nn) 
                 a.visitStart(tree,ctx)(owner)
           }
           traverseTreeChildren(tree)(owner)
           toProcess.foreach{analysises =>
              for(a <- analysises.nn) 
                 a.visitDone(tree,ctx)(owner)
              scheduledVisits.remove(tree)
           }

       }
       val enabled = allAnalysers.filter(_.enabled)
       if (!enabled.isEmpty) then
          scheduledVisits.put(tree,ListBuffer(enabled: _*))
          batchTraverse.traverseTree(tree)(Symbol.spliceOwner)
          for(a <- enabled) {
             a.afterTreeTraverse(asyncMacroFlags)
          }

     end analyzeTree
  
   end Observatory

end ObservatoryQuoteScope

object Observatory:

  trait Scope extends ObservatoryFullQuoteScope {
      val observatory: Observatory
  }

  def apply(using inQctx: Quotes)(tree: inQctx.reflect.Tree, flags: AsyncMacroFlags) =
    val scope = new Scope {

       override val qctx: Quotes = inQctx

       override val observatory = new Observatory(tree.asInstanceOf[quotes.reflect.Tree], flags)
    }
    scope.observatory
  

