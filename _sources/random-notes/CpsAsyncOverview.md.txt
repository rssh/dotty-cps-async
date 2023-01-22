## CPS-Async transformations

### CPS Overview

CPS stands for [continuation-passing style][cps].

It was described at [AI MEM 349: SCHEME: An Interpreter for Extended Lambda Calculus](https://dspace.mit.edu/handle/1721.1/5794) as a style, where a function always "returns" its result by "sending" it to another function. 

Ie in direct (usual) style we write a sequence of functions, in CPS (or better called «traditional CPS»), we pass to the first function parameter and “the rest of the program”, which do own work and then call this ‘contunuation’.

Let’s look an example:

Direct-style:

```scala
val connection = openConnection()
val command = connection.readCommand()
val result = engine.evaluateCommand(command)
result
```

Traditional continuation passing style:

```
openConnection(connection =>
   connection.read(command =>
     engine.evaluate(command,  Function.identity())
) )
```


More formally: Given the function  ```f: X =>Y```,  which transforms ```X``` to ```Y```.
Let’s build function  ``` f': X * (Y=>S) => S ```, which accepts ```X``` and a function ```Y=>S```.  The second parameter ```Y => S``` is the continuation.

[dotty-cps-async] provides similar (but not exactly the same) approach, which can be named local CPS transformation over monads. 

Example:
```
 openConnection().flatMap(connection =>
    connection.read().flatMap(command =>
      engine.evaluate(command)))
```

More formally: Given the function ```f: X => Y```, which transforms ```X``` to ```Y```.   
Let’s build function ```f’: X => M[Y]```,  where ```M``` is some monad.  
Note, that if we have the continuation function  ```g:  Y => S```, than it can be applied after ```f``` in a monadic ```flatMap``` operation:

```
Y = f(x)
S = g(y)
```

is transformed to 

```
f’(x).flatMap(y => g’(y,c))
```

* Next question: how this related to await/async ?

In Scala, concurrent execution usually is presented by some monad.  (Future, IO, Task,...  etc).
(You can look at  [A Poors man's concurrency monad](https://pdfs.semanticscholar.org/d4e0/a8554588b91f7404a75bd79807c08771da22.pdf) for background).

I.e. operations with concurrent API  often return ```M[F]```, and you can work with received value by flatMapping ‘next’  operations inside received monad.  Manually operating of  ‘in-monad’ values is low-level and error-prone.   Can we avoid this?

  Let’s introduce the two pseudo-operations

 * await:  ```M[X] => X```
 * async:  ```X => M[X]```

Async apply monad CPS to its argument.  Note, that the transformation of `await(expr)` will be `expr`.  (i.e. all occurrences of `await` will be erased).  And we can use direct style instead monadic compositions, leaving work of aligning compositions to a computer.  This is exactly what [dotty-cps-async] do.


### Optimizations

Traditional CPS async transforms all code (even sequential) to monadic compositions.
We want to keep sequential parts to be left sequential, so the number of flatMap operations inside async block is equal to the number of the really concurrent operations.
 To achieve this, we maintain data structures ([`CpsExpr`][cpsexpr] and [`CpsTree`][cpstree]) which ‘remember’ that block of code is sequential and compose sequential parts without wrapping all in monad.


### Implementation notes

   * We don't do ANF transform preprocessing, but transform code as is, by providing implementation along with some micro-optimization, as the same way, as human will transform those expressions 'by hands'.
   * For chunks of code, which can be deconstructed with the help of Scala 3 'quote' expressions, we use representation of block as [`CpsExpr`][cpsexpr]. Other expressions, deconstructed as [TASTy] trees, are represented as [`CpsTree`][cpstree].
 

<!-- hyperlinks -->

[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[cpsexpr]: https://rssh.github.io/dotty-cps-async/api/jvm/cps/macros/CpsExpr.html
[cpstree]: https://rssh.github.io/dotty-cps-async/api/jvm/cps/macros/forest/CpsTreeScope$CpsTree.html
[dotty-cps-async]: https://github.com/rssh/dotty-cps-async#dotty-cps-async
[tasty]: https://docs.scala-lang.org/scala3/guides/tasty-overview.html
