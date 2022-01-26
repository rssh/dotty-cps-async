Monad Context 
=============

Monad context is a way to provide an additional API, which is available only inside some monad 
(i.e., inside appropriative |async|_ block).   
In the introduction chapter, we have shown a simplified representation of the |async|_ signature:

.. code-block:: scala

  def async[F[_], T](using am: CpsMonad[F])(expr: T) => F[T]

   
The complete definition looks like:

.. code-block:: scala

  transparent inline def async[F[_]](using am: CpsMonad[F]) =
    macros.Async.InferAsyncArg(using am)

  // and then in macros.Async:

  class InferAsyncArg[F[_], C](using val am: CpsMonad.Aux[F, C]) {

    transparent inline def apply[T](inline expr: C ?=> T) =
      // ...
       
  }


Here we split an application into two parts, to have one type parameter in |async|_; this becomes possible with the ``async[F]`` syntax.
Take a look at the argument of the ``InferAsyncArg.apply`` method: ``expr: C ?=> T``.   
This is a context function. The context parameter ``C`` is extracted from the monad definition. 
Inside ``expr`` the Scala 3 compiler makes an implicit instance of ``C`` available, which we can use to provide an internal monad API. 

The complete await signature lools like:

.. code-block:: scala

  def await[F[_], T, G[_]](using CpsAwaitabe[F], CpsMonadContext[G])(expr: T) => F[T]

where `F` is a type of awaited wrapper and `G` monad in enclosing |async|_ block.


Using a context parameter makes our monad a bit more complex than traditional Haskell-like monad constructions but allows us to represent important industry cases, like structured concurrency.   
Jokingly, we can say that our monad is close to the original Leibnic definition of Monadology, where each monad has unique qualities, not accessible from outside.

The monad context is defined as a type inside |CpsMonad|_ :

.. code-block:: scala

    trait CpsMonad[F[_]] ....

      type Context <: CpsMonadContext[F]
      // ...
 
    }


|CpsMonadContext|_ provides the functionality to adopt awaiting another monadic expression into the current context.
      
.. code-block:: scala

    trait CpsMonadContext[F[_]] {

      /**
       * adopt external monadic value to the current context.
       **/
      def adoptAwait[A](fa: F[A]): F[A]
 
    }



As a practical example, let's consider adding a timeout to the plain Scala future.  
I.e., let's think about how to build the monad ``FutureWithTimeout``, which will complete within a timeout or fire a 
|TimeoutException|_. It's more or less clear how to combine a small |Future|_ with timeouts into one 
(at this point, we can rename timeouts to deadlines), but what should we do when the control flow 
is waiting for completing an external |Future|_ in |await|_? The answer is the usage of a monad context:  
``adoptAwait`` can generate a promise, which will be filled in case of finishing ``f`` or elapsing timeout.  

See example |TestFutureWithDeadline.scala|_ for the implementation of such an approach.

Note that this is one variant of the code organization approach.  Alternatively, we can signal to ``f``, 
if we know that we are exclusively own ``f`` evaluation. This can be an approach for lazy effect.  
The design choice for possible solutions is quite large.

For monad writers: as a general design rule, use monad context when you want to provide access to some API, which should be visible only inside a monad (i.e. inside |await|_).  For trivial cases, when you don't need a context API, you can mix |CpsMonadInstanceContext|_ into your implementation of trait |CpsMonad|_.  
For more advanced cases, we advise using the |CpsContextMonad|_ trait.

Also, you can notice the compatibility of this context with |monadic-reflection|_, based on Flinsky encoding, where |async|_ becomes |reify|_ and |await|_ accordingly |reflect|_. 


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |CpsMonad| replace:: ``CpsMonad``
.. _CpsMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L20

.. |CpsMonadContext| replace:: ``CpsMonadContext``
.. _CpsMonadContext: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadContext.scala#L11

.. |CpsContextMonad| replace:: ``CpsContextMonad``
.. _CpsContextMonad: https://github.com/rssh/dotty-cps-async/blob/a6f2bfdf83f4ffb9985b455c57e867e3e9b8c9da/shared/src/main/scala/cps/CpsMonadContext.scala#L47

.. |CpsMonadInstanceContext| replace:: ``CpsMonadInstanceContext``
.. _CpsMonadInstanceContext: https://github.com/rssh/dotty-cps-async/blob/a6f2bfdf83f4ffb9985b455c57e867e3e9b8c9da/shared/src/main/scala/cps/CpsMonadContext.scala#L22

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |monadic-reflection| replace:: **monadic-reflection**
.. _monadic-reflection: https://github.com/lampepfl/monadic-reflection

.. |reflect| replace:: ``reflect``
.. _reflect: https://github.com/lampepfl/monadic-reflection/blob/main/core/src/main/scala/monadic/Monadic.scala#L26

.. |reify| replace:: ``reify``
.. _reify: https://github.com/lampepfl/monadic-reflection/blob/main/core/src/main/scala/monadic/Monadic.scala#L31

.. |TimeoutException| replace:: ``TimeoutException``
.. _TimeoutException: https://www.scala-lang.org/api/current/scala/concurrent/index.html#TimeoutException=java.util.concurrent.TimeoutException

.. |TestFutureWithDeadline.scala| replace:: ``TestFutureWithDeadline.scala``
.. _TestFutureWithDeadline.scala: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/test/scala/cps/context/ftm/TestFutureWithDeadline.scala
