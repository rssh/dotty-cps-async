
## CPS-Async transformations: 

### Overview.

CPS - stand for continuation-passing style. (https://en.wikipedia.org/wiki/Continuation-passing_style ).

It was described at [AI MEM 349: SCHEME: An Interpreter for Extended Lambda Calculus](https://dspace.mit.edu/handle/1721.1/5794) as a style, where a function always "returns" its result by "sending" it to another function. 

Ie in direct (usual style)  we write a sequence of functions, in CPS (or better call this traditional CPS), we pass to the first function parameter and “the rest of the program”, which do own work and then call this ‘contunuation’.

Let’s look an example:

Direct-style:

```scala
val  connection = openConnection()
val  command = connection.readCommand()
val  result = engine.evaluateCommand(command)
result
```

Traditional continuation passing style:

```
openConnection(connection =>
   connection.read(command =>
     engine.evaluate(command,  Function.identity())
) )
```


More formally: let we have function  ```f: X =>Y```,  which transform ```X``` to ```Y```.  
Let’s build function  
 ``` f': X * (Y=>S) => S ```, which accept ```X``` and function ```Y=>S```.  This second parameter:  ```Y => S``` is continuation.

Dotty-cps-async provides similar (but not exactly the same) approach, which can be named local cps transformation over monads. 

Example:
```
 openConnection().flatMap(connection =>
    connection.read().flatMap(command =>
      engine.evaluate(command)))
```

More formally, let we have function ```f: X => Y```, which transform ```X``` to ```Y```.   
Let’s build function ```f’: X => M[Y]```,  where ```M``` is some monad.  
Note, that if we have continuation function  ```g:  Y => S```, than it can be applied after ```f``` in monadic ```flatMap``` operation:

```
Y = f(x)
S = g(y)
```

Will be transformed to 

```
f’(x).flatMap(y => g’(y,c))
```

* Next question: how this related to await/async ?

In scala, concurrent execution usually is presented by some monad.  (Future, IO, Task,...  etc).
(You can look at  [A Poors man's concurrency monad](https://pdfs.semanticscholar.org/d4e0/a8554588b91f7404a75bd79807c08771da22.pdf) for background).

I.e. operations with concurrent API  often return ```M[F]```, and you can work with received value by flatMapping ‘next’  operations inside received monad.  Manually operating of  ‘in-monad’ values is low-lever and error-prone.   Can we avoid this?

  Let’s introduce pseudo-operation 

 * await:  ```M[X] => X```
 * async:  ```X => M[X]```

Async apply monad CPS to it’s argument.  Note, that the transformation of ```await(expr)``` will be ```expr```.  (i.e. all occurrences of await will be erased).  And we can use direct style instead monadic compositions, leaving work of aligning compositions to a computer.  This is exactly wat [dotty-cps-async](https://github.com/rssh/dotty-cps-async#dotty-cps-async) do.


### Optimizations.

Traditional CPS async transforms all code (even sequential) to monadic compositions.
We want to keep sequential parts to be left sequential, so the number of flatMap operations inside async block is equal to the number of the really concurrent operations.
 To achieve this, we maintain data structures (```CpsExpr``` and ```CpsTree```) which ‘remember’ that block of code is sequential and compose sequential parts without wrapping all in monad.


### Implementation notes.

   * We don't do ANF transform preprocessing, but transform code as is, by providing implementation along with some micro-optimization, as the same way, as human will transform those expressions 'by hands'.
   * For chunks of code, which can be deconstructed with help of dotty 'quote' expressions, we use representation of block as 'CpsExpr'. Other expressions, deconstructed as tasty trees and represented as ```CpsTree```.
 


