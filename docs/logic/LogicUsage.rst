LogicStream: Monadic Logic Programming
======================================

`dotty-cps-async-logic` is an additional library for programming with backtracking in scala 3.

Note, that logic programming here is understanded in limited sence, as a programming with backtracking.
Embedding of full logic language is not in the current scope.


Dependency
----------

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "dotty-cps-async-logic" % "<version>"

Basic Usage
-----------

Simple example:

.. code-block:: scala

   import cps._
   import cps.monads.logic.*

   def primes: LogicStream[Int] = {
     eratosphen(2, TreeSet.empty[Int])
   }

   def eratosphen(c:Int, knownPrimes: TreeSet[Int]): LogicStream[Int] = reify[LogicStream]{
        guard(
          knownPrimes.takeWhile(x => x*x <= c).forall(x => c % x != 0)
        )
        c
   } |+| eratosphen(c+1, knownPrimes + c)

   val first100 = primes.toLazyList.take(100).toSeq


dotty-cps-asyn-logic provides a monad typeclasses for logic programming:
 -   `CpsLogicMonad <https://github.com/rssh/dotty-cps-async/blob/master/logic/shared/src/main/scala/cps/monads/logic/CpsLogicMonad.scala#L18>`_ basic logical operators.
 -   `CpsSyncLogicMonad <https://github.com/rssh/dotty-cps-async/blob/master/logic/shared/src/main/scala/cps/monads/logic/CpsSyncLogicMonad.scala>`_ specialization for synchronous style of programming.
 -   `CpsConcurrentLogicMonad <https://github.com/rssh/dotty-cps-async/blob/master/logic/shared/src/main/scala/cps/monads/logic/CpsConcurrentLogicMonad.scala>`_ monad transformer over concurrent monad.

And implementation
   - `LogicStreamT <https://github.com/rssh/dotty-cps-async/blob/master/logic/shared/src/main/scala/cps/monads/logic/LogicStreamT.scala>`_.
   - `LazyList based implementation <https://github.com/rssh/dotty-cps-async/blob/master/logic/shared/src/main/scala/cps/monads/logic/LazyListCpsLogicMonad.scala>`_.


Logical operators
------------------


Basically LogicStream is a computation, which produces a lazy sequence of values. API follows the interface of
haskell LogicT monad transformer, described in `Backtracking, Interleaving, and Terminating
Monad Transformers <https://okmij.org/ftp/papers/LogicT.pdf>`_.

The main operators are described below.
Complete API documentation is avaialable `here <api/logic/jvm/cps/monads/logic.html>`_.


seqOr
*****

.. code-block:: scala

    extension (x:M[A]) def seqOr(y:M[A]): M[A]

We can compose two such computations using `|+|` operator, with meaning of concatenation of argument streams.
(appropriative alphanumeric method is 'seqOr' ).

.. code-block:: scala

    def fib(n1:Int, n2:Int): LogicStream[Int] = reify[LogicStream]{
       n1
    } |+| fib(n2, n1+n2)

    val fibs = fib(1,1).toLazyList.take(10).toSeq
    // fibs == Seq(1,1,2,3,5,8,13,21,34,55)


`seqOr` implements backtracking with search in depth.  In the next expression

.. code-block:: scala

    fib(1,1) |+| primes


We will get a sequence of fibonacci and `primes` will never be observer.


interleave:
***********

.. code-block:: scala

    extension (x:M[A]) def interleave(y:M[A]): M[A]


or some cases it is more appropriate to use search in width.
For this we can use `interleave` operator, often named as 'fair or'. (appropriative symbolic name is '|' ).

.. code-block:: scala

    fib(1,1) | primes   //  synonim is  fib(1,1) interleave primes

will produce sequence of fibonacci and primes interleaved.


once
****

.. code-block:: scala

    extension (x:M[A]) def once: M[A]

Once return a stream with only first value (or empty stream if original stream is empty).
The usage of ònce` allows to reduce the search space.  Appropriative functionality in Prolog is `cut`.

.. code-block:: scala

    def firstPrime: LogicStream[Int] = primes.once

    val first = firstPrime.toLazyList.headOption
    // first == Some(2)

ifThenElse
**********

.. code-block:: scala

    extension (cond:M[Boolean]) def ifThenElse[T](thenBranch: =>M[T], elseBranch: =>M[T]): M[T]

ifThenElse is like a conditional operator, but value are logical streams.  It takes three arguments: condition, then branch and else branch.
If `cond` is not empty, then `thenBranch` is flatMapped, otherwise `elseBranch` is streamed.


Inside of monadic brackets we can use `guard`, `choices` and `fail` operators.

guard
*****

.. code-block:: scala

    inline def guard[M[_]](cond:Boolean)(using CpsLogicMonadContext[M]): Unit

guard takes a boolean condition and returns empty stream if condition is false, and stream with single value `()` if condition is true.

.. code-block:: scala

    def evens(input: LogicStream[Int]): LogicStream[Int] = reify[LogicStream]{
       val n = reflect(input)
       guard(n%2==0)
       n
    }

    val evenNumbers = even(nat).toLazyList.take(10).toSeq
    // evenNumbers == Seq(2,4,6,8,10,12,14,16,18,20)


choices
*******

.. code-block:: scala

    inline def choices[M[_],A](xs:A*)(using CpsLogicMonadContext[M]): A


Explore all choices from an arguments.

.. code-block:: scala

    def choiceTest: LogicStream[Int] = reify[LogicStream]{
       val x = choices(1,2,3)
       x*2
    }

    val choiceTestResult = choiceTest.toLazyList.toSeq
    // choiceTestResult == Seq(2,4,6)


Synchronous style
------------------

`CpsSyncLogicMonad` typeclass is a specialization of `CpsLogicMonad` for synchronous style of programming
with the abiltiy to transform output result to LazyList.

Asynchronous style
------------------

`CpsConcurrentLogicMonad` defines a monad transformer over concurrent monad, which can stream result
into asynchronous stream and define `parOr` operator.

parOr
*****

.. code-block:: scala

    extension (x:M[A]) def parOr(y:M[A]): M[A]

`parOr` is a parallel or operator, which can be used to compose two computations in parallel.
The result stream will contain values from both streams, interleaved in some order.
