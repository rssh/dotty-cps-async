Monads interoperability.
========================

Monads in async and await can be different:  ``await[F]`` can be applied inside ``async[G]``  when exists 
`CpsMonadConversion[F, G] <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadConversion.scala>`_.

Future Examples
---------------

``async[F]{ await[Future](.. ) }``
..................................

Here is an example of implementation of ``Conversion`` from ``Future`` to any async monad ``G[_]`` :


.. code-block:: scala

 given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]): 
                                                  CpsMonadConversion[Future,G] with
     def apply[T](ft:Future[T]): G[T] =
           summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )


Here 'async monad' for ``G[_]`` means that it is possible to receive ``G[T]`` from a callback, which returns ``T``.


.. code-block:: scala

 trait CpsAsyncMonad[F[?]] extends CpsTryMonad[F] {

   /**
    * called by the source, which accept callback.
    * source is called immediatly in adoptCallbackStyle
    **/
   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A]

 }


After making this definition available, we can await Future into any async monad:


.. code-block:: scala

 def fun(x:Int):Future[Int] =
       Future successful (x+1)

     val c = async[ComputationBound]{
       val a = await(fun(10))
       a
     }


``async[Future]{ await[F](.. ) }``
..................................

And how about inserting ``await[F]`` into  ``async[Future]`` ?.
 For this it should mean, that our ``F`` should be able to schedule operation:

.. code-block:: scala

 trait CpsSchedulingMonad[F[?]] extends CpsAsyncMonad[F] {

   /**
    * schedule execution of op somewhere.
    * Note, that characteristics of scheduler can vary.
    **/
   def spawn[A](op: =>F[A]): F[A]

 }


This can be immediatly evaluation for imperative monads, or for monads with delayed evaluation, 
like haskell-like IO -- submittiong op to a pull, which should be evaluated during ```unsafePerformIO``` 
at the end of the world.

You can read implementation of conversion of scheduled monad to ``Future`` in  `FutureAsyncMonad.scala <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/FutureAsyncMonad.scala>`_ 

Of course, it is is possible to create other conversions between you monads, based on other principles.

js.Promise
-----------

Not only monads can be subject to await. For example, it is impossible to attach monad structure to ``js.Promise`` in scalajs, 
because map operation is unimplementable: all ``Promise`` operation flatten their arguments.  But we can await ``Promise`` from scala
``async[Future]`` blocks, because ``CpsMonadConversion[Future,Promise]`` is defined.

Also, for fluent implementation of JS facades, dotty-cps-async provides ``JSFuture`` trait, which has monadic operations in scala and visible from JavaScript as ``Promise``.  
i.e. with the following definitions:

.. code-block:: scala

 import cps.monads.jsfuture.{given,*}

 @JSExportTopLevel("FromScalaExample")
 object FromScalaExample:

   @JSExport
   def myFunction(x: String): JSFuture[String] = async[JSFuture] {
       .... // can use await from futures and promises
   }


``FromScalaExampl.myFunction("string")``  can be used as ``Promise`` on javascript side.



