Automatic Coloring 
===================

Overview
-------- 

Sometimes, especially when we work with distributed systems, most API calls are asynchronous and should be prefixed by |await|_.  And we should remember what functions we should call async and what - not.  It is known as 'async coloring problem': i.e. we should split our code technically into two parts (colors):  one works with async expressions (i.e., ``F[T]``) and one - sync. (``T`` without ``F``).

If we want to put an asynchronous expression into a synchronous function, we should write ``await(expr)`` instead of ``expr``, for transforming the synchronous code into an asynchronous one
(see |What Color is Your Function?|_ for more detailed explanation).

In |Scala 3|_, we have types, so why not ask the compiler to do async coloring automatically?
So, the code below

.. code-block:: scala

  async[Future] {
    val url = "http://www.example.com"
    val data = await(api.fetchUrl("http://www.example.com"))
    val theme = api.classifyText(data)
    val dmpInfo: String = await(api.retrieveDMPInfo(url, await(theme), "1"))
    dmpInfo
  }


can be written without |await|_ as:

.. code-block:: scala

   import cps.automaticColoring.given  

   val c = async[Future] {
     val url = "http://www.example.com"
     val data = api.fetchUrl("http://www.example.com")
     val theme = api.classifyText(data)
     val dmpInfo: String = api.retrieveDMPInfo(url, theme, "1")
     dmpInfo
   }


Automatic Coloring & Memoization
--------------------------------

If the underlying monad supports execution caching for using this feature (i.e., two awaits on the same expression should not cause reevaluation), then implicit await is enough for automatic coloring.  But should we handle pure effect monads, which hold computations without starting them?


Let's look at the following code:

.. code-block:: scala

  def updateCounter(counter: Counter) = async[IO] {
    val value = counter.increment()
    if value % LOG_MOD == 0 then
      log(s"counter value = ${await(value)}")
    if (value - 1 == LOG_TRESHOLD) then
      // Conversion will not be appliyed for == . 
      // For this example we want automatic conversion, so -1
      log("counter TRESHOLD")
  }


Assume IO is some pure effect monad, which holds a computation function that will be evaluated each time we need to get a value from the monad. ``Counter.increment()`` is an IO action:  


.. code-block:: scala

  def Counter:

    def increment(): IO[Int]


The compiler will insert awaits when testing and printing value. 
For making two using of ``val value`` be the same value, we need to memoize value during the creation of ``val``. 
Otherwise, the counter will be incremented three times instead of one.

Signature of memoization operation can be different for different monads; this can be:
* ``memoize: F[X] => F[X]`` for imperative monads.
* ``memoize: F[X] => F[F[X]]`` for pure effect monads.  Internal ``F[_]`` holds cached computation. External ``F[_]`` - operation of getting a result from received cache. Flattening this expressin will return the original computation.


If we want to provide support for automatic coloring for your monad, you should implement the |CpsMonadMemoization[F]|_ trait, which can be one of:
* ``CpsMonadMemoization.Default`` - if computations are cached in your monad by default.
* ``CpsMonadMemoization.Inplace`` - for imperative monads
* ``CpsMonadMemoization.Pure`` - for pure effect monads.
* ``CpsMonadMemoization.Dynamic`` - for monads with custom memoization, which resolved with call-side types.


Safety rules for using memoized effect.
---------------------------------------

Safety rules for variable memoization are enforced with the help of additional preliminary analysis. If some variable is used only in a synchronous context (i.e., via |await|_), it should be colored as synchronous (i.e., cached). If some variable is passed to other functions as effect - it should be colored asynchronous (i.e., uncached). If the variable is used in both synchronous and asynchronous contexts, we can't deduce the programmer’s intention and report an error.

Preliminary analysis using next algorithm:

* For each invocation of a variable inside |async|_ block - count the number of calls with and without awaits.
* If we have a call with await, then using the same variable in ia call without await reported as an error (and vice-versa)
* If the variable, defined outside of the async block, is used in synchronous context more than once - the macro also will report an error.


Custom value discard
--------------------

.. index:: customValueDiscard

During the writing of asynchronous code, typical developers’ mistakes are to forget to handle something connected with discarded values, like error processing or awaiting.  

``cps.customValueDiscard`` limits the value discarding in the non-final expression in the block.  When enabled, value discarding is allowed only for those types ``T``, for which it exists an implementation of a special |ValueDiscard[T]|_.

- If given |ValueDiscard[T]|_ is not found in the current scope, then dropping values of this type is prohibited.
- If found ``ValueDiscard.apply(t)`` is called. The method is defined as a no-op for primitive types and can be extended by the developer for its own types.

Example:

Assume we have next api:

.. code-block:: scala

 object api:
   def fetch(url: String): Future[String]
   def dryRun(data: String): Future[Unit] 
   def processData(data: String): Future[String]
 
Where the semantics of ``dryRun``  - raise an error if it is impossible to run ``processData()``.

Let's look at the next code:

.. code-block:: scala

 //import cps.customValueDiscard.given  // < 0.9.3
 import cps.customValueDiscard

 val c = async[Future] {
   val data = await(api.fetch("http://www.example.com"))
   dryRun(data)
   await(process(data))
 } 


Here the developer forgot to wrap ``dryRun`` into |await|_.  But ``customValueDiscard`` feature is enabled and value discard operation is not defined for |Future|_, so this code will not compile.

.. index:: warnValueDiscard

If you want to see warning instead error, you can import `warnValueDiscard` feature:

.. code-block:: scala

 //import cps.warnValueDiscard.given  //  < 0.9.3
 import cps.warnValueDiscard


Note that custom value discarding is automatically enabled for effect monads to prevent situations where discarding values drop branches in the computation flow. Let's look again at the code:

.. code-block:: scala

  def updateCounter(counter: Counter) = async[IO] {
    val value = counter.increment()
    if value % LOG_MOD == 0 then
      log(s"counter value = ${await(value)}")
    if value - 1 == LOG_TRESHOLD then
      // Conversion will not be appliyed for == . For this example we want automatic conversion, so -1
      log("counter TRESHOLD")
  }

Assuming that logging is an IO operation, i.e. function ``log`` has the signature

.. code-block:: scala

   def log(message: String): IO[Unit]


Without custom value discarding, the log statement will be dropped.  (Type of ``if`` with one branch is |Unit|_, so type of the first branch should be |Unit|_ and the ``log`` statement will be discarded).
|dotty-cps-async|_ provides special |AwaitValueDiscard|_  which forces the monad to be evaluated before being discarded.  We recommend to use this discard as default for ``IO[Unit]``.


.. rubric:: Footnotes

.. [#f1] The definitions of |async|_ and |await|_ are simplified, in reality they are more complex, because we want to infer the type of the expression independently from the type of monad.


.. ###########################################################################
.. ## Hyperlink definitions with text formatting (e.g. verbatim, bold)

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |AwaitValueDiscard| replace:: ``AwaitValueDiscard``
.. _AwaitValueDiscard: https://github.com/rssh/dotty-cps-async/blob/ff25b61f93e49a1ae39df248dbe4af980cd7f948/shared/src/main/scala/cps/ValueDiscard.scala#L44

.. |CpsMonadMemoization[F]| replace:: ``CpsMonadMemoization[F]``
.. _CpsMonadMemoization[F]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonadMemoization.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |Scala 3| replace:: **Scala 3**
.. _Scala 3: https://dotty.epfl.ch/

.. |Unit| replace:: ``Unit``
.. _Unit: https://www.scala-lang.org/api/current/scala/Unit.html

.. |ValueDiscard[T]| replace:: ``ValueDiscard[T]``
.. _ValueDiscard[T]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/ValueDiscard.scala#L11

.. |What Color is Your Function?| replace:: **What Color is Your Function?**
.. _What Color is Your Function?: https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/
