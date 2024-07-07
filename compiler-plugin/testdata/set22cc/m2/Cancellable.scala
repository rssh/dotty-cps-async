package gears.async

trait Cancellable:

  private[gears] var optGroup: Option[CompletionGroup] = None

  //  bug in scalajs: stack overflow when use unlinked.
  private def group: CompletionGroup = optGroup.getOrElse(CompletionGroup.Unlinked)
  private def group_=(group: CompletionGroup): Unit = optGroup = Some(group)

  def cancel(): Unit

  def link(group: CompletionGroup): this.type = synchronized:
    this.group.drop(this)
    this.group = group
    this.group.add(this)
    this

  /** Link this cancellable to the cancellable group of the current async context.
   */
  def link()(using async: Async): this.type =
    link(async.group)

  /** Unlink this cancellable from its group. */
  def unlink(): this.type =
    link(CompletionGroup.Unlinked)


end Cancellable


object Cancellable:

  trait Tracking extends Cancellable:

    def isCancelled: Boolean


  object Tracking:

    def apply() = new Tracking:
      private var cancelled: Boolean = false

      def cancel(): Unit =
        cancelled = true

      def isCancelled = cancelled

  end Tracking

end Cancellable
