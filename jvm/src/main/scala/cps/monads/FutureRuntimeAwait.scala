package cps.monads

import cps.*
import cps.runtime.*
import scala.concurrent.*

object FutureRuntimeAwait extends LoomRuntimeAwait[Future]