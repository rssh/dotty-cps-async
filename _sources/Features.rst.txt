Additional Features
===================

Short syntax for ``await``
--------------------------

It can be helpful when monad or environment does not support automatic coloring, but the default |await|_ syntax is too heavy.  For this case, we define the |unary_!|_ operator for use instead of |await|_. 

Example:

.. code-block:: scala

    import cps.syntax.`unary_!`

    val x = username + !fetchToken(data)


Inside the |async|_ block this will be a synonym for

.. code-block:: scala

    val x = username + await(fetchToken(data))


SIP-22 compatible interface
---------------------------

.. index:: sip22

This feature provides a compatibility layer for Scala 2 |SIP-22|_ (implemented in |scala-async|_). 
When migrating your program from legacy SIP-22 to Scala 3, you can change the imports from

.. code-block:: scala

 import scala.async.Async.{async, await}

to

.. code-block:: scala

 import cps.compat.sip22.{async, await}

and use |Future|_-based async/await.

All test cases from the original |scala-async|_ distribution are passed with a change of imports only,
and included in our regression suite.

It is also possible to compile |SIP-22|_ code without changing the source code with |shim--scala-async--dotty-cps-async|_'s help. 

.. code-block:: scala

 libraryDependencies += "com.github.rssh" %% "shim-scala-async-dotty-cps-async" % "0.9.7",


Note that compatibility was not a primary goal during the development of |dotty-cps-async|_. The generated code is quite different, so if you need a bug-to-bug compatible version of Scala 2 |scala-async|_, you should use the port of the original ``-XAsync`` compiler plugin.


.. ###########################################################################
.. ## Hyperlink definitions with text formatting (e.g. verbatim, bold)

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |SIP-22| replace:: **SIP-22 async**
.. _SIP-22: https://docs.scala-lang.org/sips/async.html

.. |scala-async| replace:: ``scala-async``
.. _scala-async: https://github.com/scala/scala-async

.. |shim--scala-async--dotty-cps-async| replace:: ``shim--scala-async--dotty-cps-async``
.. _shim--scala-async--dotty-cps-async: https://github.com/rssh/shim--scala-async--dotty-cps-async

.. |unary_!| replace:: ``unary_!``
.. _unary_!: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/syntax/package.scala
