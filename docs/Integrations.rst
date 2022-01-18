.. _Integrations:

Integrations
============

|dotty-cps-async|_ itself provides type classes for monads, available without external dependencies: this is  |FutureAsyncMonad|_ ,  JVM-only Java |CompletableFuture|_ and JS-only |js.Promise|_  and |JSFuture|_ .

 
Third-party effect stacks are provided in external modules.
 

rssh/cps-async-connect
----------------------

GitHub project: https://github.com/rssh/cps-async-connect


Cats Effect
^^^^^^^^^^^

Add dependency |cps-async-connect-cats-effect|_ to your ``build.sbt`` to integrate |Cats Effect|_ :

 .. code-block:: scala

  libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.4"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-cats-effect" % "0.9.5"


**Note**: Typelevel's project |cats-effect-cps|_ also provides async/await syntax support for |Cats Effect|_.


Monix
^^^^^

Add dependency |cps-async-connect-monix|_ to your ``build.sbt`` to integrate |Monix|_ :

 .. code-block:: scala

  libraryDependencies += "io.monix" %% "monix" % "3.4.0"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-monix" % "0.9.5"


Scalaz IO
^^^^^^^^^

Add dependency |cps-async-connect-scalaz|_ to your ``build.sbt`` to integrate |Scalaz IO|_ :

 .. code-block:: scala

  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-scalaz" % "0.9.5"


ZIO and ZIO Streams
^^^^^^^^^^^^^^^^^^^

Add dependency |cps-async-connect-zio|_ to your ``build.sbt`` to integrate |ZIO|_ :

 .. code-block:: scala

  libraryDependencies += "dev.zio" %% "zio" % "2.0.0-RC1"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-zio" % "0.9.5"


Akka Streams
^^^^^^^^^^^^

Add dependency |cps-async-connect-akka-stream|_ to your ``build.sbt`` to integrate Lightbend's |Akka Streams|_ :

 .. code-block:: scala

  libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.18"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-akka-stream" % "0.9.5"


FS2 Stream
^^^^^^^^^^

Add dependency |cps-async-connect-fs2|_ to your ``build.sbt`` to integrate Typelevel's |FS2|_ :

 .. code-block:: scala

  libraryDependencies += "co.fs2" %% "fs2-core" % "3.2.0"
  libraryDependencies += "co.fs2" %% "fs2-io" % "3.2.0"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-fs2" % "0.9.5"


typelevel/cats-effect-cps
-------------------------

GitHub project: https://github.com/typelevel/cats-effect-cps

|cats-effect-cps|_ is an experimental library to support uniform async/await syntax for |Cats Effect|_ in Scala 2 and Scala 3, integrated with |Typelevel ecosystem|_.


Call for additions:
-------------------

If you have implemented |CpsMonad|_ support for some effect stack and want to mention it here - please, send a pull request about this.


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |Akka Streams| replace:: **Akka Streams**
.. _Akka Streams: <https://doc.akka.io/docs/akka/current/stream/

.. |Cats Effect| replace:: **Cats Effect**
.. _Cats Effect: https://typelevel.org/cats-effect/

.. |cats-effect-cps| replace:: ``cats-effect-cps``
.. _cats-effect-cps: https://github.com/typelevel/cats-effect-cps

.. |CompletableFuture| replace:: ``CompletableFuture``
.. _CompletableFuture: https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/main/scala/cps/monads/CompletableFutureCpsMonad.scala

.. |cps-async-connect-akka-stream| replace:: ``cps-async-connect-akka-stream``
.. _cps-async-connect-akka-stream: https://github.com/rssh/cps-async-connect#akka-streams

.. |cps-async-connect-cats-effect| replace:: ``cps-async-connect-cats-effect``
.. _cps-async-connect-cats-effect: https://github.com/rssh/cps-async-connect#cats-effect

.. |cps-async-connect-fs2| replace:: ``cps-async-connect-fs2``
.. _cps-async-connect-fs2: https://github.com/rssh/cps-async-connect#fs2-streams

.. |cps-async-connect-monix| replace:: ``cps-async-connect-monix``
.. _cps-async-connect-monix: https://github.com/rssh/cps-async-connect#monix

.. |cps-async-connect-scalaz| replace:: ``cps-async-connect-scalaz``
.. _cps-async-connect-scalaz: https://github.com/rssh/cps-async-connect#scalaz-io

.. |cps-async-connect-zio| replace:: ``cps-async-connect-zio``
.. _cps-async-connect-zio: https://github.com/rssh/cps-async-connect#zio

.. |CpsMonad| replace:: ``CpsMonad``
.. _CpsMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |FS2| replace:: **FS2**
.. _FS2: https://fs2.io/

.. |FutureAsyncMonad| replace:: ``FutureAsyncMonad``
.. _FutureAsyncMonad: https://https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala

.. |JSFuture| replace:: ``JSFuture``
.. _JSFuture: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/jsfuture/JSFuture.scala

.. |js.Promise| replace:: ``js.Promise``
.. _js.Promise: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/PromiseCpsAwaitable.scala

.. |Monix| replace:: **Monix**
.. _Monix: https://monix.io/

.. |Scalaz IO| replace:: **Scalaz IO**
.. _Scalaz IO: https://scalaz.github.io/

.. |Typelevel ecosystem| replace:: **Typelevel ecosystem**
.. _Typelevel ecosystem: https://typelevel.org/cats/typelevelEcosystem.html

.. |ZIO| replace:: **ZIO**
.. _ZIO: https://zio.dev/
