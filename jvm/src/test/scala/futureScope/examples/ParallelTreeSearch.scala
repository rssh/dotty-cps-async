package futureScope.examples

import scala.concurrent.*

import cps.*
import cps.monads.{*,given}
import cps.testconfig.given

import futureScope.*

/**
 * Motivation Example for joinads.
 * See 
 * Tomas Petricek and Don Syme. 2011. 
 * Joinads: a retargetable control-flow construct for reactive, parallel and concurrent programming. 
 * In Proceedings of the 13th international conference on Practical aspects of declarative languages (PADL'11). 
 * Springer-Verlag, Berlin, Heidelberg, 205–219.
 *
 * https://dl.acm.org/doi/10.5555/1946313.1946336
 */


/**
 * Binary tree with ordering, we want to search for data in this tree in parallel.
 */
enum BinaryTree[+T:Ordering] {
  case Empty extends BinaryTree[Nothing]
  case Node[T:Ordering](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

}

object BinaryTree {

  import scala.concurrent.ExecutionContext.Implicits.global

  def findFirst[T:Ordering](tree: BinaryTree[T], p: T=>Boolean): Future[Option[T]] = async[Future].in(Scope) {
    val eventFlow = EventFlow[T]()
    val runner = findFirstInContext(tree,eventFlow,p,0)
    await(eventFlow.events.next)
  }


  def findFirstInContext[T:Ordering](tree: BinaryTree[T], events: EventFlow[T], p: T=> Boolean, level: Int)(using FutureScopeContext): Future[Unit] = {
   async[Future]{
      tree match
        case BinaryTree.Empty => 
        case BinaryTree.Node(value, left, right) =>
          if (p(value)) then
            events.post(value)
          else 
            val p1 = FutureScope.spawn( findFirstInContext(left, events, p, level+1) )
            val p2 = FutureScope.spawn( findFirstInContext(right, events, p, level+1) )
            await(p1)
            await(p2)
      if (level == 0) then
        events.finish()
    }
  }


}