
Generators
===================

Async Streaming
---------------

Generator syntax is a way to deliver values into some form of a stream asynchronously.


Example:

.. code-block:: scala
  import cps.*
  import cps.monads.{*,given}
  import cps.stream.*

  asyncStream[AsyncList[Future,Int]]{ out =>
     out.emit(0)
     for(i <- 1 to 10)
       out.emit(i)
  }


Here `AsyncList <https://rssh.github.io/dotty-cps-async/api/jvm/api/cps/stream.AsyncList.html>`_ is a minimal implementation of async stream supplied with dotty-cps-async.
Exists integration modules for well-known async streaming libraries (see :ref:`Integrations`).

The input to asyncStream is a block of code, which should be a lambda-expression that accepts an emitter argument.
I.e., simplified definition looks next:

.. code-block:: scala

   inline def asyncStream[R](using a: CpsAsyncAbsorber[R])(f: CpsAsyncEmitter[a.Stream, a.Monad, a.Element) => Unit): R


For a full definition, look at the source:
  - `asyncStream <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/AsyncStream.scala>`_  - entry point for macro
  - `CpsAsyncEmitAbsorber <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/CpsAsyncEmitAbsorber.scala>`_  - is an adapter from a generator to a stream of the given type.
  - `CpsAsyncEmitter <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/CpsAsyncEmitter.scala>`_ is a trait with operations `emit`, which should be called inside asyncStream or async block. 


Writing generator adapters for custom streams
--------------------------------------------
 
To allow generator syntax for your stream, you need to implement 
 `CpsAsyncEmitAbsorber[R] <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/stream/CpsAsyncEmitter.scala#L46>`_ where `evalAsync` accept cps-transformed function and output the result stream.
 
Dotty-cps-async provide a platform-specific trait `BaseUnfoldCpsAsyncEmitAbsorber` which can simplicify generator implementations for streams which has something like `unfoldAsync[S,E](s:S)(f:S => F[Option[(S,E)]]):R`.

For example, looks at the implementation of CpsAsyncEmitAbsorber for Akka-stream Source:

.. code-block:: scala

   given AkkaStreamEmitAbsorber[T](using ExecutionContext):  
                                BaseUnfoldCpsAsyncEmitAbsorber[Source[T,NotUsed],Future,T] with 

      override type Element = T

      def unfold[S](s0:S)(f:S => Future[Option[(T,S)]]): Source[T, NotUsed] =
        Source.unfoldAsync[S,T](s0)((s) => f(s).map(_.map{ case (x,y) => (y,x) }) )

