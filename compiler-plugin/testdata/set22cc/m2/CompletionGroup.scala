package gears.async

import cps.*

class CompletionGroup extends Cancellable.Tracking:

  def isCancelled: Boolean = ???

  def cancel(): Unit = ???

  /** Wait for all members of the group to complete and unlink themselves. */
  private[async] def waitCompletion()(using AsyncContext): Unit = {
    ???
  }

  /** Add given member to the members set. If the group has already been cancelled, cancels that member immediately. */
  def add(member: Cancellable): Unit =
     ???

  /** Remove given member from the members set if it is an element */
  def drop(member: Cancellable): Unit = 
     ???


object CompletionGroup:

  /** A sentinel group of cancellables that are in fact not linked to any real group. `cancel`, `add`, and `drop` do
    * nothing when called on this group.
    */
  object Unlinked extends CompletionGroup:
    override def cancel(): Unit = ()
    override def waitCompletion()(using CpsDirect[JSAsync]): Unit = ()
    override def add(member: Cancellable): Unit = ()
    override def drop(member: Cancellable): Unit = ()
  end Unlinked

end CompletionGroup
