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

  libraryDependencies += "org.typelevel" %% "cats-effect" % catsEffectVersion
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-cats-effect" % cpsAsyncConnectVersion

|Cats Effect|_ GitHub : |typelevel/cats-effect|_, Maven : |org.typelevel»cats-effect|_.

**Note**: Typelevel's project |cats-effect-cps|_ also provides async/await syntax support for |Cats Effect|_.


Monix
^^^^^

Add dependency |cps-async-connect-monix|_ to your ``build.sbt`` to integrate |Monix|_ :

 .. code-block:: scala

  libraryDependencies += "io.monix" %% "monix" % "3.4.1"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-monix" % cpsAsyncConnectVersion

|Monix|_ GitHub : |monix/monix|_, Maven : |io.monix|_.

Scalaz IO
^^^^^^^^^

Add dependency |cps-async-connect-scalaz|_ to your ``build.sbt`` to integrate |Scalaz IO|_:

 .. code-block:: scala

  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.4.0-M12"
  libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.4.0-M12"
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-scalaz" % cpsAsyncConnectVersion

|Scalaz IO|_ GitHub : |scalaz/scalaz|_, Maven : |org.scalaz|_.

ZIO and ZIO Streams
^^^^^^^^^^^^^^^^^^^

Add dependency |cps-async-connect-zio|_ to your ``build.sbt`` to integrate |ZIO|_ :

For 1.0.x series:

 .. code-block:: scala

  libraryDependencies += "dev.zio" %% "zio" % zio1Version
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-zio" % cpsAsyncConnectVersion

For 2.0.x series:

 .. code-block:: scala

  libraryDependencies += "dev.zio" %% "zio" % zio2Version
  libraryDependencies += "com.github.rssh" %% "cps-async-connect-zio2" % cpsAsyncConnectVersion


|ZIO|_ GitHub: |zio/zio|_, Maven : |dev.zio|_.


Akka Stream
^^^^^^^^^^^

Add dependency |cps-async-connect-akka-stream|_ to your ``build.sbt`` to integrate Lightbend's |Akka Stream|_ :

 .. code-block:: scala

  libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-akka-stream" % cpsAsyncConnectVersion

|Akka Stream|_ GitHub : |akka/akka|_, Maven : |com.typesafe.akka»akka-stream|_.

FS2 Stream
^^^^^^^^^^

Add dependency |cps-async-connect-fs2|_ to your ``build.sbt`` to integrate Typelevel's |FS2|_ :

 .. code-block:: scala

  libraryDependencies += "co.fs2" %% "fs2-core" % fs2Version
  libraryDependencies += "co.fs2" %% "fs2-io" % fs2Version
  libraryDependencies += "com.github.rssh" %%% "cps-async-connect-fs2" % cpsAsyncConnectVersion

|FS2|_ GitHub : |typelevel/fs2|_, Maven : |co.fs2|_.


Probability Monad
^^^^^^^^^^^^^^^^^

Add dependency |cps-async-connect-probability-monad|_ to your ``build.sbt``.



typelevel/cats-effect-cps
-------------------------

GitHub project: https://github.com/typelevel/cats-effect-cps

|cats-effect-cps|_ is an experimental library to support uniform async/await syntax for |Cats Effect|_ in Scala 2 and Scala 3, integrated with the |Typelevel ecosystem|_.


Call for additions:
-------------------

If you have implemented |CpsMonad|_ support for some effect stack and want to mention it here - please, send a |pull request|_ about this.


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |Akka Stream| replace:: **Akka Stream**
.. _Akka Stream: <https://doc.akka.io/docs/akka/current/stream/

.. |akka/akka| replace:: ``akka/akka``
.. _akka/akka: https://github.com/akka/akka

.. |Cats Effect| replace:: **Cats Effect**
.. _Cats Effect: https://typelevel.org/cats-effect/

.. |cats-effect-cps| replace:: ``cats-effect-cps``
.. _cats-effect-cps: https://github.com/typelevel/cats-effect-cps

.. |co.fs2| replace:: ``co.fs2``
.. _co.fs2: https://mvnrepository.com/artifact/co.fs2

.. |com.typesafe.akka»akka-stream| replace:: ``com.typesafe.akka»akka-stream``
.. _com.typesafe.akka»akka-stream : https://mvnrepository.com/artifact/com.typesafe.akka/akka-stream

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

.. |cps-async-connect-probability-monad| replace:: ``cps-async-connect-probability-monad``
.. _cps-async-connect-probability-monad: https://github.com/rssh/cps-async-connect#probability-monad


.. |CpsMonad| replace:: ``CpsMonad``
.. _CpsMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L20

.. |dev.zio| replace:: ``dev.zio``
.. _dev.zio: https://mvnrepository.com/artifact/dev.zio

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |FS2| replace:: **FS2**
.. _FS2: https://fs2.io/

.. |io.monix| replace:: ``io.monix``
.. _io.monix: https://mvnrepository.com/artifact/io.monix

.. |FutureAsyncMonad| replace:: ``FutureAsyncMonad``
.. _FutureAsyncMonad: https://https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala

.. |JSFuture| replace:: ``JSFuture``
.. _JSFuture: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/jsfuture/JSFuture.scala

.. |js.Promise| replace:: ``js.Promise``
.. _js.Promise: https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/PromiseCpsAwaitable.scala

.. |Monix| replace:: **Monix**
.. _Monix: https://monix.io/

.. |monix/monix| replace:: ``monix/monix``
.. _monix/monix: https://github.com/monix/monix

.. |org.scalaz| replace:: ``org.scalaz``
.. _org.scalaz: https://mvnrepository.com/artifact/org.scalaz

.. |org.typelevel»cats-effect| replace:: ``org.typelevel»cats-effect`` 
.. _org.typelevel»cats-effect : https://mvnrepository.com/artifact/org.typelevel/cats-effect

.. |pull request| replace:: pull request
.. _pull request: https://github.com/rssh/dotty-cps-async/pulls

.. |Scalaz IO| replace:: **Scalaz IO**
.. _Scalaz IO: https://scalaz.github.io/

.. |scalaz/scalaz| replace:: ``scalaz/scalaz``
.. _scalaz/scalaz: https://github.com/scalaz/scalaz

.. |Typelevel ecosystem| replace:: **Typelevel ecosystem**
.. _Typelevel ecosystem: https://typelevel.org/cats/typelevelEcosystem.html

.. |typelevel/cats-effect| replace:: ``typelevel/cats-effect`` 
.. _typelevel/cats-effect : https://github.com/typelevel/cats-effect

.. |typelevel/fs2| replace:: ``typelevel/fs2``
.. _typelevel/fs2: https://github.com/typelevel/fs2
.. |ZIO| replace:: **ZIO**
.. _ZIO: https://zio.dev/

.. |zio/zio| replace:: ``zio/zio``
.. _zio/zio: https://github.com/zio/zio
