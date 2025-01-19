package cps.monads

import cps.CpsRuntimeAwait
import cps.runtime.*

import scala.concurrent.Future
import java.util.concurrent.CompletableFuture

given CpsRuntimeAwait[Future] = FutureLoomRuntimeAwait

given CpsRuntimeAwait[CompletableFuture] = CompletableFutureRuntimeAwait
