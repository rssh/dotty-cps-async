# References


## Presentations

Embedding generic monadic transformers into scala.

* TFP2022 slides: <https://www.slideshare.net/rssh1/embedding-generic-monadic-transformer-into-scala-tfp2022> (May 2022)
    * article: <https://link.springer.com/chapter/10.1007/978-3-031-21314-4_1> 
    * preprint: <https://arxiv.org/abs/2209.10941>

Can we free concurrent programming from the monadic style:

* ScalaR:  <https://www.youtube.com/watch?v=ImlUuTQUeaQ>  (June 2020)
* ScalaUA: <https://www.youtube.com/watch?v=w-noRPLxYoA>  (April 2020)
    * slides: <https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style>

## Related work in Scala 2

- Scala-continuations.  paper:  <https://infoscience.epfl.ch/record/149136/files/icfp113-rompf.pdf>
- Scala-async:   <https://github.com/scala/scala-async>
- Storm-enroute coroutines:  <https://drops.dagstuhl.de/opus/volltexte/2018/9208/pdf/LIPIcs-ECOOP-2018-3.pdf>
- Thoughtworks DSL.scala:  <https://github.com/ThoughtWorksInc/Dsl.scala>
- Monadless.io: <http://monadless.io/>
- Effectful: <https://github.com/pelotom/effectful>
- Scala-gopher tech report: <https://arxiv.org/abs/1611.00602>
   
## Related work in Scala 3
 
 - Monadic-reflection <https://github.com/lampepfl/monadic-reflection>  (requires a [project Loom](https://openjdk.java.net/projects/loom/) enabled JVM)
-  Thoughtworks DSL.scala recently ported to Scala 3:  <https://github.com/ThoughtWorksInc/Dsl.scala> 
-  zio-direct (direct translation of a subset of scala on top of ZIO effect system) <https://github.com/zio/zio-direct>

## Related work in other languages

- Historical overview: <https://softwareengineering.stackexchange.com/questions/377464/who-did-async-await-first>

<!-- in alphabetic order -->

- [ANSI C++][ansi_cplusplus]:
	- N4134 (C++17 proposal (implemented but deferred) )  <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4134.pdf>
	- N4680 (C++20 stackless coroutines proposal)  <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/n4680.pdf>
	- Blog  <https://lewissbaker.github.io/2017/11/17/understanding-operator-co-await>
	- Imlplementation of C++ coroutines:  <https://github.com/lewissbaker/cppcoro>
- [C#](https://docs.microsoft.com/en-us/dotnet/csharp/):
   - C# Asynchronous programming: <https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/>
   - Midori project (Async OS, written in C#): <http://joeduffyblog.com/2015/11/03/blogging-about-midori/>
- [Dart]:
	- Guide: <https://dart.dev/guides/language/language-tour#asynchrony-support>
	- Formal specs: <https://spec.dart.dev/DartLangSpecDraft.pdf>  (async intro on page 18)
    - Spicing Up Dart with Side Effects (streams extension) <https://dl.acm.org/doi/pdf/10.1145/2742694.2747873>
- Firefly:
	- Blog post about analog of automatic coloring: https://www.ahnfelt.net/async-await-inference-in-firefly/
- [F#][fsharp]:
    - F# Computation Expression: <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions>
	- Paper: <http://tomasp.net/academic/papers/computation-zoo/>
        - Joinad language extension (paper): http://tomasp.net/academic/papers/joinads/joinads.pdf
        - F# Computation Expression Zoo (paper): http://tomasp.net/academic/papers/computation-zoo/computation-zoo.pdf
- [Go][golang]:
   - [Concurrency in Go][golang_concurrency].
- [Java]:
  - [On Parallelism and Concurrency][pressler] by Ron Pressler, November 2021.
- [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript): 
   - async function: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function>
   - [**funfix-effect**][funfix-effect] - monadic data types for dealing with laziness and side effects.
- [Koka]:
	- Paper: "Structured Asynchrony with Algebraic Effects" <https://www.microsoft.com/en-us/research/wp-content/uploads/2017/05/asynceffects-msr-tr-2017-21.pdf>
- [Kotlin]:
	- [Kotlin Coroutines Guide][kotlin_coroutines].
- [Nim]:
   - Nim macro module <https://nim-lang.org/docs/asyncdispatch.html>
- [OCaml]:
    - Paper: "Concurrent System Programming with Effect Handlers": <https://kcsrk.info/papers/system_effects_feb_18.pdf>
    - Effects tutorial <https://github.com/ocamllabs/ocaml-effects-tutorial> (CUFP'17)
    - Paper: Retrofitting Effect Handlers onto OCaml. <https://arxiv.org/abs/2104.00250>
- [Python 3][python3]:
   - PEP-0492 - Coroutines with async and await syntax: <https://www.python.org/dev/peps/pep-0492/>
   - [Trio] - a friendly Python 3 library for async concurrency and I/O.
- [Rust]:
	- Async-Await working group: <https://rust-lang.github.io/compiler-team/working-groups/async-await/>
	- Book "Asynchronous Programming in Rust": <https://rust-lang.github.io/async-book/>
	- Rust language reference: <https://doc.rust-lang.org/nightly/reference/expressions/await-expr.html>
   - [Tokio](https://tokio.rs/tokio/tutorial) - an asynchronous runtime for [Rust].
- [Swift]:
	- Proposal: <https://github.com/apple/swift-evolution/blob/main/proposals/0296-async-await.md>
- [Zig]:
	- Description: <https://ziglang.org/download/0.5.0/release-notes.html#Async-Functions>
	- Article about doing async implicit ('colorblind'): <https://kristoff.it/blog/zig-colorblind-async-await/>


## Monadic Computations in Functional Programming (unrelated to PO Syntax, most examples are Haskell).

   - Extending monads via pattern matching (joinads for haskell): <http://tomasp.net/academic/papers/docase/docase.pdf>
   - ‘do’ Unchained: Embracing Local Imperativity in a Purely Functional Language (Lean): <https://leanprover.github.io/papers/do.pdf>
                                                               


<!-- hyperlinks -->

[ansi_cplusplus]: https://isocpp.org/
[dart]: https://dart.dev/codelabs/async-await
[fsharp]: https://fsharp.org/
[funfix-effect]: https://funfix.org/api/effect/
[golang]: https://go.dev/
[golang_concurrency]: https://www.golang-book.com/books/intro/10
[java]: https://docs.oracle.com/javase/specs/
[koka]: https://koka-lang.github.io/
[kotlin]: https://kotlinlang.org/
[kotlin_coroutines]: https://kotlinlang.org/docs/reference/coroutines/coroutines-guide.html
[nim]: https://nim-lang.org/
[ocaml]: https://ocaml.org/
[pressler]: https://inside.java/2021/11/30/on-parallelism-and-concurrency/
[python3]: https://www.python.org/
[rust]: https://trio.readthedocs.io/
[swift]: https://developer.apple.com/swift/
[trio]: https://trio.readthedocs.io/
[zig]: https://ziglang.org/
