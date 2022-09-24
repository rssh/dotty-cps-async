package cps


/**
 * Marker typeclass for wrappers, which we can await.
 * Such traits can be not monads itself (for example, its impossible to set monad structure over js.Promise)
 * but can be convertable into cps monads.
 **/
 trait CpsAwaitable[F[_]] 