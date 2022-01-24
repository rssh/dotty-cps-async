# dotty-cps-async


This is the implementation of async/await transformation for [Scala 3][scala3] (Dotty), based on an optimized version of [cps] (*continuation passing style*) transformation, where continuation is ‘pushed’ to the monad. 

## Highlightings

 * Full support of all Scala language constructs in async/await block.
 * Pluggable monad interface:  
    *  An await monad can be any trait for which it is possible to implement [`CpsAsyncMonad`](https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala) type class. You can provide those methods for your favorite monad.
 * Limited support of high-order functions:
    * ```urls.map(fetchData(_))(await _ )``` is an idiomatic way to fetch data for all items in parallel.
    * An application developer or library author can provide 'shifted' implementation of the own high-order functions.
 * Optional features, which enhance ergonomics in some cases, such as automatic coloring and handling discarded values
 * Optional [SIP-22 compatible API][sip_22].

For more details, please, read the documentation at https://rssh.github.io/dotty-cps-async/.


## Presentations

* Scala3 & Async: Behind Futures.
   * Svitla Smart Talk. https://www.youtube.com/watch?v=fBcGhjM2s-c (Apr. 2021. In Ukrainian language).

* Can we free concurrent programming from the monadic style?
    * ScalaR:  https://www.youtube.com/watch?v=ImlUuTQUeaQ  (Jun 2020)
    * ScalaUA: https://www.youtube.com/watch?v=w-noRPLxYoA&t=3s  (Apr. 2020)
   (slides: https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style )

[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[scala3]: https://dotty.epfl.ch/
[sip_22]: https://docs.scala-lang.org/sips/async.html
