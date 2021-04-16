# dotty-cps-async


This is the implementation of async/await transformation for Dotty (next version of the Scala programming language), based on an optimized version of cps[continuation passing style] transformation, where continuation is ‘pushed’ to the monad. 

## Highlightings:

 * Full support of all scala language constructs in async/await block.
 * Pluggable monad interface:  
    *  An await monad can be any trait, for which it is possible to implement [CpsAsyncMonad](https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala) typeclass. You can provide those methods for your favorite monad.
 * Limited support of high-order functions:
    * ```urls.map(fetchData(_))(await _ )```  is an idiomatic way to fetch data for all items in parallel.
    * An application developer or library author can provide 'shifted' implementation of the own high-order functions.
 * Optional features, which enhance ergonomics in some cases, such as automatic coloring and handling discarded values
 * Optional SIP-22 compatible API.

For more details, please, look at the documentation: https://rssh.github.io/dotty-cps-async/

Currently, doty-cps-async is at an early stage and not ready for production use.  There are many things, which not work yet.   You can help to develop by providing test cases and implementing missing parts.  

## Presentations

* Scala3 & Async: Behing Futures.
   * Svitla Smart Talk. https://www.youtube.com/watch?v=fBcGhjM2s-c (Apr. 2021. In Ukrainian language).

* Can we free concurrent programming from the monadic style:
    * ScalaR:  https://www.youtube.com/watch?v=ImlUuTQUeaQ  (Jun 2020)
    * ScalaUA: https://www.youtube.com/watch?v=w-noRPLxYoA&t=3s  (Apr. 2020)
   (slides: https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style )


   
