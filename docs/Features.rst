Additional Features
===================

Short syntax for await
----------------------

It can be helpful when monad or environment does not support automatic coloring, but the default `await` syntax is too heavy.  For this case, we define `unary_!` operator for use instead of `await`. 

Example:

.. code-block:: scala

    import cps.syntax.`unary_!`

    val x = username + !fetchToken(data)


Inside the async block this will be a synonim for

.. code-block:: scala

    val x = username + await(fetchToken(data))





SIP22-compatible interface
----------------------------

.. index:: sip22

This feature provides a compatibility layer for Scala2 `SIP-22 <https://docs.scala-lang.org/sips/async.html>`_ 
`async <https://github.com/scala/scala-async>`_. 
When migrating your program from legacy SIP22 to dotty, you can change the headers, from

.. code-block:: scala

 import scala.async.Async.{async,await}

to

.. code-block:: scala

 import cps.compat.sip22.{async,await}

and use Future based async/await.

All test cases from the original Scala-Async distribution are passed with a change of imports only,
and included in our regression suite.

It is also possible to compile sip22 async code without changing of the source code with `shim--scala-async--dotty-cps-async <https://github.com/rssh/shim--scala-async--dotty-cps-async>`_ -s help. 

.. code-block:: scala

 libraryDependencies += "com.github.rssh" %% "shim-scala-async-dotty-cps-async" % "0.9.4",


Note that compatibility was not a primary goal during the development of dotty-cps-async. Generated code is quite different, so if you need a bug-to-bug compatible version of scala2 async, you should use the port of the original -XAsync compiler plugin.



