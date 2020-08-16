Monads interoperability.
========================

Monads in async and await can be different:  ``await[F]`` can be applied inside ``async[G]``  when exists ``CpsMonadConversion[F, G]`` (see `CpsMonadConversion.scala <https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/CpsMonadConversion.scala>`_ ).

Future Examples
---------------

``async[F]{ await[Future](.. ) }``
..................................

Here is an example of implementation of ``CpsMonadConversioni`` from ``Future`` to any async monad ``G[_]`` :


.. code-block:: scala

 given fromFutureConversion[G[_]](using ExecutionContext, CpsAsyncMonad[G]) as CpsMonadConversion[Future,G] =
   new CpsMonadConversion[Future, G] {
     override def apply[T](mf: CpsMonad[Future], mg: CpsMonad[G], ft:Future[T]): G[T] =
           summon[CpsAsyncMonad[G]].adoptCallbackStyle(
                                         listener => ft.onComplete(listener) )
   }


Here 'async monad' for ```G[_]`` means that it is possible to receive ``G[T]`` from a callback, which returns ``T``.


.. code-block:: scala

 trait CpsAsyncMonad[F[_]] extends CpsTryMonad[F] {

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

And how about inserting ``await[F]``` into  ``async[Future]``` ?.
 For this it should mean, that our ``F`` should be able to schedule operation:

.. code-block:: scala

 trait CpsSchedulingMonad[F[_]] extends CpsAsyncMonad[F] {

   /**
    * schedule execution of op somewhere.
    * Note, that characteristics of scheduler can vary.
    **/
   def spawn[A](op: =>F[A]): F[A]

 }


This can be immediatly evaluation for imperative monads, or for monads with delayed evaluation, 
like haskell-like IO -- submittiong op to a pull, which should be evaluated during ```unsafePerformIO``` 
at the end of the world.

You can read implementation of conversion of scheduled monad to ```Future`` in  `FutureAsyncMonad.scala <https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/FutureAsyncMonad.scala>`_ 

Of course, it is is possible to create other conversions between you monads, based on other principles.


