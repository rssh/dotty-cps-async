
Generators
==========

Async Streaming
---------------

The generator syntax is a way to deliver values into some form of a stream asynchronously.


Example:

.. code-block:: scala

  import scala.concurrent.{Await, Future}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration
  import cps.*                  // asyncStream, await
  import cps.monads.{*, given}  // support for build-in monads (i.e. Future)
  import cps.stream.*           // AsyncList

  object Example:
  
    def main(args: Array[String]): Unit =
      val stream = asyncStream[AsyncList[Future, Int]] { out =>
        out.emit(0)
        for i <- 1 to 10 do out.emit(i)
      }
      val f = stream.takeListAll()
      val res = Await.result(f, Duration(1, "seconds"))
      println(s"res=$res")


I recommend you try |AsyncList|_.

Here |AsyncList|_ is a minimal implementation of async stream supplied with |dotty-cps-async|_.
There exist integration modules for well-known async streaming libraries (see section :ref:`Integrations`).

The input to |asyncStream|_ is a block of code, which should be a lambda-expression that accepts an emitter argument; i.e., the simplified definition looks as follows :

.. code-block:: scala

   inline def asyncStream[R](using a: CpsAsyncAbsorber[R])(f: CpsAsyncEmitter[a.Stream, a.Monad, a.Element) => Unit): R


For a full definition, look at the source:

- |asyncStream|_ is the entry point for macro
- |CpsAsyncEmitAbsorber[R]|_ is an adapter from a generator to a stream of the given type.
- |CpsAsyncEmitter|_ is a trait with operations ``emit``, which should be called inside |asyncStream|_ or |async|_ block. 


Writing generator adapters for custom streams
---------------------------------------------
 
To allow generator syntax for your stream, you need to implement 
 |CpsAsyncEmitAbsorber[R]|_ where ``evalAsync`` accepts a cps-transformed function and outputs the result stream.
 
|dotty-cps-async|_ provides a platform-specific trait |BaseUnfoldCpsAsyncEmitAbsorber|_ which can simplicify generator implementations for streams which has something like ``unfoldAsync[S, E](s: S)(f:S => F[Option[(S, E)]]): R``.

For example, look at the implementation of |CpsAsyncEmitAbsorber[R]|_ for |Akka Streams|_ source:

.. code-block:: scala

   given AkkaStreamEmitAbsorber[T](using ExecutionContext):  
                                BaseUnfoldCpsAsyncEmitAbsorber[Source[T, NotUsed], Future, T] with 

     override type Element = T

     def unfold[S](s0: S)(f: S => Future[Option[(T, S)]]): Source[T, NotUsed] =
       Source.unfoldAsync[S, T](s0)((s) => f(s).map(_.map{ case (x, y) => (y, x) }) )


.. ###########################################################################
.. ## Hyperlink definitions with text formatting (e.g. verbatim, bold)

.. |Akka Streams| replace:: **Akka Streams**
.. _Akka Streams: https://doc.akka.io/docs/akka/current/stream/

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |AsyncList| replace:: ``cps.stream.AsyncList``
.. _AsyncList: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/AsyncList.scala

.. |asyncStream| replace:: ``asyncStream``
.. _asyncStream: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/AsyncStream.scala#L20

.. |BaseUnfoldCpsAsyncEmitAbsorber| replace:: ``BaseUnfoldCpsAsyncEmitAbsorber``
.. _BaseUnfoldCpsAsyncEmitAbsorber: https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/main/scala/cps/stream/BaseUnfoldCpsAsyncEmitAbsorber.scala#L10

.. |CpsAsyncEmitAbsorber[R]| replace:: ``CpsAsyncEmitAbsorber[R]``
.. _CpsAsyncEmitAbsorber[R]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/CpsAsyncEmitAbsorber.scala

.. |CpsAsyncEmitter| replace:: ``CpsAsyncEmitter``
.. _CpsAsyncEmitter: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/CpsAsyncEmitter.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async
