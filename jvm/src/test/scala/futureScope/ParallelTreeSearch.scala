package futureScope

import scala.concurrent.*

import cps.*
import cps.monads.{*,given}


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
   //implicit val printCode = cps.macros.flags.PrintCode 
   //implicit val debugLevel = cps.macros.flags.DebugLevel(10)
   async[Future]{
      tree match
        case BinaryTree.Empty => 
        case BinaryTree.Node(value, left, right) =>
          if (p(value)) {
             events.post(value)
          }
          val p1 = FutureScope.spawn( findFirstInContext(left, events, p, level+1) )
          val p2 = FutureScope.spawn( findFirstInContext(right, events, p, level+1) )
          if (level == 0) {
            FutureScope.spawn{
              await(p1)
              await(p2)
              events.finish()
            }
          }
   }
  }


}