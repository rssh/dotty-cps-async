# References

## Presentations

Can we free concurrent programming from the monadic style:

* ScalaR:  <https://www.youtube.com/watch?v=ImlUuTQUeaQ>  (Jun 2020)
* ScalaUA: <https://www.youtube.com/watch?v=w-noRPLxYoA>  (Apr. 2020)
    * slides: <https://www.slideshare.net/rssh1/can-concurrent-functional-programming-be-liberated-from-monadic-style>

## Related work in Scala2

- Scala-continuations.  paper:  <http://infoscience.epfl.ch/record/149136/files/icfp113-rompf.pdf>
- Scala-async:   <https://github.com/scala/scala-async>
- Storm-enroute coroutines:  <https://drops.dagstuhl.de/opus/volltexte/2018/9208/pdf/LIPIcs-ECOOP-2018-3.pdf>
- Thoughtworks DSL.scala:  <https://github.com/ThoughtWorksInc/Dsl.scala>
- Monadless.io: <http://monadless.io/>
- Effectfull: <https://github.com/pelotom/effectful>
- Scala-gopher tech report: <https://arxiv.org/abs/1611.00602>
   
## Related work in other languages

- Historical overview: <https://softwareengineering.stackexchange.com/questions/377464/who-did-async-await-first>

- F# Computation Expression. 
	- Guide: <https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions>
	- Paper: <http://tomasp.net/academic/papers/computation-zoo/>
- C# : Guide:  <https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/task-asynchronous-programming-model>
- Python: PEP-0492  <https://www.python.org/dev/peps/pep-0492/>
- JavaScript:  <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function>
- Nim: macro module <https://nim-lang.org/docs/asyncdispatch.html>
- Dart:
	- Guide: <https://dart.dev/guides/language/language-tour#asynchrony-support>
        - Formal specs: <https://spec.dart.dev/DartLangSpecDraft.pdf>  (async intro on page 18)
- Rust:
	- Async-Await working group: <https://rust-lang.github.io/compiler-team/working-groups/async-await/>
        - Book "Asynchronous Programming in Rust": <https://rust-lang.github.io/async-book/>
        - Rust language reference: <https://doc.rust-lang.org/nightly/reference/expressions/await-expr.html>
- C++:
	- N4134 (C++17 proposal (implemented but deferred) )  <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4134.pdf>
        - N4680 (C++20 stackless coroutines proposal)  <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/n4680.pdf>
        - Blog  <https://lewissbaker.github.io/2017/11/17/understanding-operator-co-await>
	- Imlplementation of C++ coroutines:  <https://github.com/lewissbaker/cppcoro>
- Kotlin:
	- Coroutines Guide: <https://kotlinlang.org/docs/reference/coroutines/coroutines-guide.html>
- Swift:
	- Proposal: <https://github.com/apple/swift-evolution/blob/main/proposals/0296-async-await.md>

