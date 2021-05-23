Integrations
============

 dooty-cps-async itself provide type classes for monads, available without external dependencies: this is  `Future <https://https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala>`_ ,  JVM-only Java `CompletableFuture <https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/main/scala/cps/monads/CompletableFutureCpsMonad.scala>`_ and JS-only `js.Promise <https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/PromiseCpsAwaitable.scala>`_  and `JSFuture <https://github.com/rssh/dotty-cps-async/blob/master/js/src/main/scala/cps/monads/jsfuture/JSFuture.scala>`_ .

 
 Third-party effect stacks are provided in external modules.
 
cps-async-connect
-----------------

 github project: https://github.com/rssh/cps-async-connect


cats-effect
^^^^^^^^^^^


 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-cats-effect" % "0.6.0"


monix
^^^^^

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-monix" % "0.6.0"


scalaz IO
^^^^^^^^^

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-scalaz" % "0.6.0"


ZIO
^^^

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-zio" % "0.6.0"



Call for additions:
-------------------

If you have implemented CpsMonad support for some effect stack and want to mention it here - please, send a pull request about this.


