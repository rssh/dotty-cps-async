package FutureGroup

import cps.*
import cps.monads.{*,given}
import cps.stream.{*,given}

import scala.concurrent.*



trait FutureGroup[E] {

   def events: AsyncIterator[Future, E]

}

object FutureGroup {

}