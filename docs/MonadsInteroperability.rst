Monads interoperability.
========================

Monads in ``async`` and ``await`` can be different: ``await[F]`` can be applied inside ``async[G]``  when exists 
|CpsMonadConversion[F, G]|_.

``Future`` Examples
-------------------

``async[F]{ await[Future]( /*..*/ ) }``
.......................................

We first describe how to invoke ``await[Future]`` inside a ``async[F]`` block.

Here is an example of implementation of ``Conversion`` from ``Future`` to any async monad ``G[_]`` :


.. code-block:: scala

 given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]): 
                                                  CpsMonadConversion[Future, G] with
   def apply[T](ft: Future[T]): G[T] =
     summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )


Here 'async monad' for ``G[_]`` means it is possible to receive ``G[T]`` from a callback, which returns ``T``.


.. code-block:: scala

 trait CpsAsyncMonad[F[?]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediatly in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A] => Unit) => Unit): F[A]

 }


After making this definition available, we can await |Future|_ into any async monad:


.. code-block:: scala

 def fun(x:Int):Future[Int] =
   Future successful (x+1)

 val c = async[ComputationBound] {
   val a = await(fun(10))
   a
 }


``async[Future]{ await[F]( /*..*/ ) }``
.......................................

And how about inserting ``await[F]`` into a ``async[Future]`` block ?

For this it should mean, that our ``F`` should be able to schedule operation:

.. code-block:: scala

 trait CpsSchedulingMonad[F[?]] extends CpsAsyncMonad[F] {

   /**
    * schedule execution of op somewhere.
    * Note, that characteristics of scheduler can vary.
    **/
   def spawn[A](op: => F[A]): F[A]

 }


This can be immediatly evaluated for imperative monads, or for monads with delayed evaluation, 
like Haskell-like IO -- submitting the ``op`` argument to a pull, which should be evaluated during ``unsafePerformIO`` at the end of the world.

You can read implementation of conversion of scheduled monad to |Future|_ in the source file |FutureAsyncMonad.scala|_.

Of course, it is possible to create other conversions between your monads, based on other principles.

js.Promise
-----------

Not only monads can be subject to await. For example, it is impossible to attach monad structure to ``js.Promise`` in scalajs, 
because map operation is unimplementable: all ``Promise`` operations flatten their arguments.  But we can await ``Promise`` from Scala
``async[Future]`` blocks, because ``CpsMonadConversion[Future, Promise]`` is defined.

Also, for fluent implementation of JS facades, |dotty-cps-async|_ provides the |JSFuture|_ trait, which has monadic operations in Scala and visible from JavaScript as ``Promise``.  
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

.. |CpsMonadConversion[F, G]| replace:: ``CpsMonadConversion[F, G]``
.. _CpsMonadConversion[F, G]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadConversion.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |FutureAsyncMonad.scala| replace:: ``FutureAsyncMonad.scala``
.. _FutureAsyncMonad.scala: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala

.. |JSFuture| replace:: ``JSFuture``
.. _JSFuture: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/jsfuture/JSFuture.scala

.. |Promise| replace:: ``Promise``
.. _Promise: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise#description
