Additional Features
===================

Automatic Coloring 
------------------


Sometimes, especially when we work with distributes systems, most of our API call are asynchronous and should be prefixed by `await`.  And we should remember what functions we should call as async and what - not.  It is known as 'async coloring problem': i.e. we should split our code technically into two parts (colors):  one works with async expressions (i.e.,, F[T]) and one - sync. (T without F).

If we want to put asynchronous expression into synchronous function, we should write `await(expr)`  instead `expr`,  for transforming synchronous code into asynchronous.
(see http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/ for more detailed explanation )


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

   import cps.automaticColoring.given  

   val c = async[Future]{
        val url = "http://www.example.com"
        val data = api.fetchUrl("http://www.example.com")
        val theme = api.classifyText(data)
        val dmpInfo: String = api.retrieveDMPInfo(url, theme, "1")
        dmpInfo
     }


If the underlying monad supports execution caching for using this feature (i.e., two awaits on the same expression should not cause reevaluation), then implicit await is enough for automatic coloring.  But what to do with pure effect monads, which holds computations without starting them?

Let's look on the next code:

.. code-block:: scala


  def updateCounter(counter:Counter) = async[IO]{
    val value = counter.increment()
    if value % LOG_MOD == 0 then
       log(s"counter value = ${await(value)}")
    if (value - 1 == LOG_TRESHOLD) then
       // Conversion will not be appliyed for == . 
       // For this example we want automatic conversion, so -1
       log("counter TRESHOLD")
  }


Assume IO is some pure effect monad, which holds a computation function that will be evaluated each time we need to get value from the monad. Counter.increment() is an IO action:  


.. code-block:: scala

  def Counter:

    def increment(): IO[Int]


The compiler will insert awaits when testing and printing value. 
For making two using of `val value` be the same value, we need to memoize value during the creation of `val`. 
Otherwise, the counter will be incremented three times instead of one.

Signature of memoization operation can be different for different monads, this can be:
   * `memoize: F[X] => F[X]`  for imperative monads.
   * `memoize: F[X] => F[F[X]]`  for pure effect monads.  Internal `F[_]` holds cached computation. External `F[_]` - operation of getting a result from received cache. Flattening this expressin will return the original computation.


If we want to provide support for automatic coloring for your monad, you should implement CpsMonadMemoization trait, which can be one of:
 * CpsMonadDefaultMemoization - if computations are cached in your monad by default.
 * CpsMonadInplaceMemoization - for imperative monads
 * CpsMonadPureMemoization - for pure effect monads.
 * CpsMonadDynamicMemoization - for monads with custom memoization, which resolved with call-side types.

Note, that automatic coloring for monads wich is not memoized by default (i.e. all effect monads) is hightly experimental and
 likely will be changed in future.


Coloring rules are following:

 * If some variable is used only in a synchronous context (i.e., via await), the macro will color it as synchronous (i.e., cached if used more than once). 
 * If some variable is passed to other functions as effect - it is colored as asynchronous (i.e., uncached).   
 * If the variable is used in synchronous and asynchronous contexts simultaneously, we can't deduce the programmer’s intention, and the coloring macro will report an error. 
 * If the variable, defined outside of the async block, is used in synchronous context more than once - the macro also will report an error.



Custom value discard
--------------------

.. index:: customValueDiscard

During the writing of asynchronous code, typical developers’ mistakes are to forget to handle something connected with discarded values, like error processing or awaiting.  

``cps.customValueDiscard``  limit the value discarding in the non-final expression in the block.  When enabled, value discarding is allowed only for those types T, for which exists an implementation of a special ValueDiscard[T]. If given ValueDiscard[T] is not found in the current scope, then dropping values of this type is prohibited.  If found - ValueDiscard.apply(t) is called. It's defined as a no-op for primitive types and can be extended by the developer for its own types.

Example:

Assume we have next api:

.. code-block:: scala

 object api:
   def  fetch(url: string): Future[String]
   def  dryRun(data:string): Future[Unit] 
   def  processData(data:string): Future[String]
 
Where the semantics of `dryRun`  - raise an error if it is impossible to run processData().

Let's look at the next code:

.. code-block:: scala

 import cps.customValueDiscard.given 

 val c = async[Future] {
    val data = await(api.fetch("http://www.example.com"))
    dryRun(data)
    await(process(data))
 } 


Here developer forgott to wrap ``dryRun`` in ``await.``  But ``customValueDiscard`` feature is enabled and value discard operation is not defined for ```Future``, so this code will not compile.

.. index:: warnValueDiscard

If you want to see warning instead error, you can import `warnValueDiscard` feature:

.. code-block:: scala

 //import cps.feature.warnValueDiscard.given  //  < 0.6.1
 import cps.warnValueDiscard.given

Note that custom value discarding is automatically enabled for effect monads to prevent situations where discarding values
 drop branches in the computation flow.
Let's look again at the code:

.. code-block:: scala

  def updateCounter(counter:Counter) = async[IO]{
    val value = counter.increment()
    if value % LOG_MOD == 0 then
       log(s"counter value = ${await(value)}")
    if (value - 1 == LOG_TRESHOLD) then
       // Conversion will not be appliyed for == . For this example we want automatic conversion, so -1
       log("counter TRESHOLD")
  }

Assuming that logging is IO operation, i.e. log have signature

.. code-block:: scala

   def log(message:String): IO[Unit]


Without custom value discarding, the log statement will be dropped.  (Type of `if` with one branch is 'Unit', so type of the first branch should be 'Unit', so log statement will be discarded).
Dotty-cps-async provides special `AwaitValueDiscard <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/ValueDiscard.scala#L27>`_  which force monad to be evaluated before be discarded.  We recommend use this discard as default for IO[Unit].


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

 libraryDependencies += "com.github.rssh" %% "shim-scala-async-dotty-cps-async" % "0.9.2",


Note that compatibility was not a primary goal during the development of dotty-cps-async. Generated code is quite different, so if you need a bug-to-bug compatible version of scala2 async, you should use the port of the original -XAsync compiler plugin.



