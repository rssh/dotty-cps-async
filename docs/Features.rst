Additional Features
===================

Implicit await
--------------

Sometimes, especially when we work with distributes systems, most of our API call are asynchronous, and near each API call should be prefixed y await.  Also, we should remember what functions we should call as async and what - not.  It is known as 'async coloring problem' (see http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/  )

In scala we have types, so why not to ask the compiler to do async coloring automatically?
So, next code:

.. code-block:: scala

  async[Future]{
     val url = "http://www.example.com"
     val data = await(api.fetchUrl("http://www.example.com"))
     val theme = api.classifyText(data)
     val dmpInfo: String = await(api.retrieveDMPInfo(url, await(theme), "1"))
     dmpInfo
  }


Can be written without await as:

.. code-block:: scala

   val c = async[Future]{
        import cps.features.implicitAwait.{given _}
        val url = "http://www.example.com"
        val data = api.fetchUrl("http://www.example.com")
        val theme = api.classifyText(data)
        val dmpInfo: String = api.retrieveDMPInfo(url, theme, "1")
        dmpInfo
     }


Looks at special syntax for enabling: ``{ given _}``.
For using this feature, the underlying monad should support execution caching:  i.e., two awaits on the same expression should not cause reevaluation.

To mark your monad as supporting this feature, you should define ``given cps.feature.implicitAwait.IsPossible[M]``.



Custom value discard
--------------------

.. index:: customValueDiscard

Common developers mistakes during the writing of asynchronous code are to forget to handle something connected with discarded values, like error processing or awaiting.  

``cps.feature.customValueDiscard``  limit the value discarding in the non-final expression in the block.  When enabled, value discarding is allowed only for those types T, for which exists an implementation of a special ValueDiscard[T]. If given ValueDiscard[T] is not found in the current scope, then dropping values of this type is prohibited.  If found - ValueDiscard.apply(t) is called. It's defined as no-op for primitive types and can be extended by developer for own types.

Example:

Assume we have next api:

.. code-block:: scala

 object api:
   def  fetch(url: string): Future[String]
   def  dryRun(data:string): Future[Unit] 
   def  processData(data:string): Future[String]
 
Where the semantics of dryRun is a raise error if it is impossible to run processData().

Let's look at the next code:

.. code-block:: scala

 import cps.features.customValueDiscard.{given _}

 val c = async[Future] {
    val data = await(api.fetch("http://www.example.com"))
    dryRun(data)
    await(process(data))
 } 


Here developer forgott to wrap ``dryRun`` in ``await.``  But ``customValueDiscard`` feature is enabled and value discard operation is not defined for ```Future``, so this code will not compile.

.. index:: warnValueDiscard

If you want to see warning instead error, you can import `warnValueDiscard` feature:

.. code-block:: scala

 import cps.features.warnValueDiscard.{given _}



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

All test cases from original Scala-Async distribution are passed with a change of imports only,
and included in our regression suite.

It is also possible to compile sip22 async code without changing of the source code with `shim--scala-async--dotty-cps-async <https://github.com/rssh/shim--scala-async--dotty-cps-async>`_ -s help. 

.. code-block:: scala

 libraryDependencies += "com.github.rssh" %% "shim-scala-async-dotty-cps-async" % "0.3.4-M2",


Note that compatibility was not a primary goal during the development of dotty-cps-async. Generated code is quite different, so if you need a bug-to-bug compatible version of scala2 async, you should use the port of the original -XAsync compiler plugin.



