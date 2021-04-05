Integrations
============

 dooty-cps-async itself provide type classes for monads, available without external dependencies: this is  `Future <https://https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/monads/FutureAsyncMonad.scala>`_ and  JVM-only Java `CompletableFuture <https://github.com/rssh/dotty-cps-async/blob/master/jvm/src/main/scala/cps/monads/CompletableFutureCpsMonad.scala>`_. 

 
 Third-party effect stacks are provided in external modules.
 

cats-effect
-----------

 Implemented in https://github.com/rssh/cps-async-connect

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-cats-effect" % "0.2.0"


scalaz IO
---------

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "cps-async-connect-cats-effect" % "0.2.0"


Call for additions:
-------------------

If you have implemented CpsMonad support for some effect stack and want to mention it here - please, send a pull request about this.


