# 🇺🇦 UKRAINE NEEDS YOUR HELP NOW!

 I'm the creator of this project and I'm Ukrainian. 
**My country, Ukraine, [is being invaded by the Russian Federation, right now](https://www.bbc.com/news/world-europe-60504334)**.
Russia is hitting target all over my country by ballistic missiles.

**Please, help to save my country!**
Ukrainian National Bank opened [an account to Raise Funds for Ukraine’s Armed Forces](https://bank.gov.ua/en/news/all/natsionalniy-bank-vidkriv-spetsrahunok-dlya-zboru-koshtiv-na-potrebi-armiyi):

 ```
 SWIFT Code NBU: NBUA UA UX
 JP MORGAN CHASE BANK, New York
 SWIFT Code: CHASUS33
 Account: 400807238
 383 Madison Avenue, New York, NY 10179, USA
 IBAN: UA843000010000000047330992708
 ```

 You can also donate to [charity supporting Ukrainian army](https://savelife.in.ua/en/donate/).

Ask you politicians:
- stop Russia access to SWIFT
- impose no-fly zone over Ukraine

# dotty-cps-async


This is the implementation of async/await transformation for [Scala 3][scala3] (Dotty), based on an optimized version of [CPS] (*Continuation Passing Style*) transformation, where the continuation is ‘pushed’ to the monad. 

## <span id="highlights">Highlights</span>

 * Full support of all Scala language constructs in async/await block.
 * Pluggable monad interface:  
    *  An await monad can be any trait for which it is possible to implement [`CpsAsyncMonad`](https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala) type class. You can provide those methods for your favorite monad.
 * Limited support of high-order functions:
    * ```urls.map(fetchData(_))(await _ )``` is an idiomatic way to fetch data for all items in parallel.
    * An application developer or library author can provide 'shifted' implementation of the own high-order functions.
 * Optional features, which enhance ergonomics in some cases, such as automatic coloring and handling discarded values
 * Optional [SIP-22 compatible API][sip_22].

For more details, please, read the documentation at https://rssh.github.io/dotty-cps-async/.


## <span id="presentations">Presentations</span>

* Scala3 & Async: Behind Futures.
   * Svitla Smart Talk. https://www.youtube.com/watch?v=fBcGhjM2s-c (April 2021. In Ukrainian language).

* Can we free concurrent programming from the monadic style?
    * ScalaR:  https://www.youtube.com/watch?v=ImlUuTQUeaQ  (June 2020)
    * ScalaUA: https://www.youtube.com/watch?v=w-noRPLxYoA&t=3s  (April 2020)
   (slides: https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style )

[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[scala3]: https://dotty.epfl.ch/
[sip_22]: https://docs.scala-lang.org/sips/async.html
