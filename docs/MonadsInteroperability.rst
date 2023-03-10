Monads interoperability.
========================

Monads in |async|_ and |await|_ can have different types, i.e. ``await[F]`` can be applied inside ``async[G]``.

We define the trait |CpsMonadConversion[F, G]|_ to support the conversion from ``F[_]`` to ``G[_]``.

``Future`` Examples
-------------------

``async[F] { await[Future]( /*..*/ ) }``
........................................

We first describe how to invoke ``await[Future]`` inside a ``async[F]`` block.

Here is an example of implementation of ``Conversion`` from |Future|_ to any async monad ``G[_]`` :


.. code-block:: scala

 given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]): 
                                                  CpsMonadConversion[Future, G] with
   def apply[T](ft: Future[T]): G[T] =
     summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )


Here, 'async monad' for ``G[_]`` means it is possible to receive ``G[T]`` from a callback, which returns ``T``.


.. code-block:: scala

 trait CpsAsyncMonad[F[?]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediately in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A] => Unit) => Unit): F[A]

 }


After making this definition available, we can await |Future|_ inside any async monad:


.. code-block:: scala

 import scala.concurrent.Future

 def fun(x: Int): Future[Int] =
   Future successful (x+1)

 val c = async[ComputationBound] {
   val a = await(fun(10))
   a
 }


``async[Future] { await[F]( /*..*/ ) }``
........................................

And how about inserting ``await[F]`` into a ``async[Future]`` block ?

For this, it means that our ``F`` should be able to schedule operation:

.. code-block:: scala

 trait CpsSchedulingMonad[F[?]] extends CpsAsyncMonad[F] {

   /**
    * schedule execution of op somewhere.
    * Note, that characteristics of scheduler may vary.
    **/
   def spawn[A](op: => F[A]): F[A]

 }


This can be immediately evaluated for imperative monads, or for monads with delayed evaluation,
like Haskell-like IO -- submitting the ``op`` argument to a pull, which should be evaluated during ``unsafePerformIO`` at the end of the world.

You can read implementation of conversion of scheduled monad to |Future|_ in the source file |FutureAsyncMonad.scala|_.

Of course, it is possible to create other conversions between your monads, based on other principles.

js.Promise
-----------

Not only monads can be subject to await. For example, it is impossible to attach the  monadic structure to |Promise|_ in |Scala.js|_, because the map operation is unimplementable: all |Promise|_ operations flatten their arguments.
But we can still await |Promise|_ from Scala ``async[Future]`` blocks, because |CpsMonadConversion[Future, Promise]|_ is defined.

Also, for a fluent implementation of JS facades, |dotty-cps-async|_ provides the |JSFuture|_ trait, which has monadic operations in Scala and visible from JavaScript as |Promise|_.  
i.e. with the following definitions:

.. code-block:: scala

 import cps.monads.jsfuture.{given,*}

 @JSExportTopLevel("FromScalaExample")
 object FromScalaExample:

   @JSExport
   def myFunction(x: String): JSFuture[String] = async[JSFuture] {
     // can use await from futures and promises
     // ...
   }


``FromScalaExample.myFunction("string")`` can be used as |Promise|_ on the JavaScript side.


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |ComputationBound| replace:: ``ComputationBound``
.. _ComputationBound: https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/test/scala/cps/ComputationBound.scala

.. |CpsMonadConversion[F, G]| replace:: ``CpsMonadConversion[F, G]``
.. _CpsMonadConversion[F, G]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadConversion.scala

.. |CpsMonadConversion[Future, Promise]| replace:: ``CpsMonadConversion[Future, Promise]``
.. _CpsMonadConversion[Future, Promise]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadConversion.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |FutureAsyncMonad.scala| replace:: ``FutureAsyncMonad.scala``
.. _FutureAsyncMonad.scala: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala

.. |JSFuture| replace:: ``JSFuture``
.. _JSFuture: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/jsfuture/JSFuture.scala#L53

.. |Promise| replace:: ``Promise``
.. _Promise: https://www.scala-js.org/api/scalajs-library/latest/scala/scalajs/js/Promise.html

.. |Scala.js| replace:: **Scala.js**
.. _Scala.js: https://www.scala-js.org/
