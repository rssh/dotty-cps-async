High-order functions.
=====================

|dotty-cps-async|_ supports the automatic transformation of high-order functions, where the lambda expression argument contains |await|_.  

For example, assume an HTTP client providing the following interface to fetch some data from a list of remote servers.

.. code-block:: scala

 trait HttpClient:
   def fetchData(url: String): Future[String] 


Then, we can fetch data from all servers just by using |await|_ in the |map|_ argument:

.. code-block:: scala

     urls.map( await(httpClient.fetchData(_)) )


Note that the default |map|_ method will run all operations sequentially. Sequential evaluation is needed to allow the code to work correctly for a case of updating the multidimensional array in a ``for`` loop with asynchronous operations.

If we want all requests to run in parallel, we can start them in one map and, when all started - wait for the end of requests:

.. code-block:: scala

       urls.map( httpClient.fetchData(_) ).map(await(_))


For handling awaits inside high-order functions, dotty-cps-async uses different strategies in dependency of execution runtime capabilities.

Async shift substitution.
-------------------------

The most general way is compile-time substitution: during async transform, |dotty-cps-async|_ substitutes method |map|_ with signature ``List[A].map[B](f: A => B)`` to  

.. index:: AsyncShift

.. code-block:: scala

 summon[AsyncShift[List[A]]].map[F[_], B](c, summon[CpsMonad[F]])
                    

which is implemented in the cps runtime with the signature

.. code-block:: scala

   AsyncShift[List[A]].map[F[_], B](obj: List[A], cpsMonad: CpsMonad[F])(f: A => F[B])


|dotty-cps-async|_ includes implementations of shifted methods for most objects of the |Scala standard library|_.

So, we can write something like

.. code-block:: scala

  val x = cache.getOrElse( await(fetchData() )


Loom-based runtime await.
-------------------------

JDK-19 includes a set of interfaces (project Loom) that allows execution of code in virtual threads, 
where runtime blocking wait is not blocking from OS view:  real thread can execute tasks from other virtual threads during the wait.   
In this case, we don't need to substitute a high-order function but change instead the function argument to the original form,
if our monad implements the `CpsRuntimeAwait <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsRuntimeAwait.scala>`_  typeclass.

This experimental feature should be enabled by declaring the implicit value of cps.macros.flags.UsingLoomAwait



How to provide shifted functions.
---------------------------------


Functional interface.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose you want to make high-order methods of your class ``C`` be able to accept lambda functions with |await|_. 
For that purpose, you have to implement the |given AsyncShift[C]|_ type class with a shifted version of your high-order methods.
Such a 'shifted' version has an additional type parameter ``F[_]`` and an additional list of arguments, inserted first, containing the original object instance and an appropriate |CpsMonad[F]|_ instance.  


Parameters should be changed in the following way:

* If the original parameter has type  ``A => B``, then changed: ``A => F[B]``
* If the original parameter is called by name with type ``=> A``, then changed: ``() => F[A]``
* Otherwise, the changed parameter has the same type as the original.


Example:

.. code-block:: scala

 case class TaggedValue[T](tag: String, value: T)
   def update[S](f: T => S): TaggedValue[S] =
     TaggedValue(tag, f(x))

 // Below the changed code:
 // - type `T => S` of argument `f` becomes `T => F[S]`
 // - `(o, m)` is prepended as the first argument list

 class TaggedValueAsyncShift[T] extends AsyncShift[TaggedValue[T]]:
   def update[F[_], S](o: TaggedValue[T], m: CpsMonad[F])(f: T => F[S]): F[TaggedValue[S]] =
     f(value).map(TaggedValue(tag,_))
             
 object TaggedValue:
   transparent inline given shiftedTaggedValue[T] as AsyncShift[TaggedValue[T] =
     TaggedValueAsyncShift[T]() 


Object-oriented interface.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases, we use classes – defined in an object-oriented manner – with private data.  If we want a class to provide an API for |dotty-cps-async|_, then we can do this without breaking encapsulation. What is needed - to implement an async-shifted version of the function inside our class:

Example:

.. code-block:: scala

 class MyIntController:
   private var x: Int = 0

   def modify(f: Int => Int): Int =
     val old = x
     x = f(x)
     sendSignal(x)
     old

   def modify_async[F[_]](m: CpsMonad[M])(f: Int => F[Int]): F[Int] =
     val old = x
     m.map(f(x))(_ => { sendSignal(x); old }) 


As we have seen, shifted functions have an additional type parameter: ``F[_]`` and a parameter |CpsMonad[F]|_ (or a more specific type, if needed).  Async transformer will substitute the call of ``modify`` into ``modify_async`` during compilation.
   Sometimes, we already have ``F[_]`` as the type parameter of the enclosing class. In such a case, we can omit those additional parameters in the async variant.

Note that you should carefully decide whether you need async function support and how to deal with concurrent modifications.  For example, in the code snippet below, different changes will interleave with each other.
 Usually, low-level constructs do not need async counterparts.


.. _substitutions-in-call-chains:

Special semantics for substitutions in call chains
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider a chain of calls, which accepts async-shifted functions.  One example is |withFilter|_ from the |Scala collections library|_.  Let's look at the following code:  

.. code-block:: scala

  for {
    url <- urls if await(status(url)) == Active
    items <- await(api.retrieveItems(url))
    item <- items
  } yield item  


Here, the usual semantics of |withFilter|_ assume that we iterate over ``urls`` only once.  But if we translate this expression according to the standard rules, we will receive two passes: one pass in async ``withFilter`` and the second pass in ``flatMap``.

To perform the iteration once, we translate ``withFilter`` not to ``F[WithFilter]`` but to a substituted type |DelayedWithFilter|_, which holds the received predicate and delays actual evaluation upon the call of the next operation in the chain.

The implementation of class |DelayedWithFilter|_ looks like:

.. code-block:: scala

 class DelayedWithFilter[F[_], A, C[X] <: Iterable[X], CA <: C[A]](
     c: CA,
     m: CpsMonad[F],
     p: A => F[Boolean],
 ) extends CallChainAsyncShiftSubst[F, WithFilter[A, C], F[WithFilter[A, C]] ]
 {
   // return eager copy
   def _finishChain: F[WithFilter[A, C]] = //...

   def withFilter(q: A => Boolean): DelayedWithFilter[F, A, CX, CA] = //...
   def withFilter_async(q: A=> F[Boolean]) = //...

   def map[B](f: A => B): F[C[B]] = //...
   def map_async[B](f: A => F[B]): F[C[B]] = //...

   def flatMap[B](f: A => IterableOnce[B]): F[C[B]] = //...
   def flatMap_async[B](f: A => F[IterableOnce[B]]): F[C[B]] = //...

   def foreach[U](f: A => U): F[Unit] = //...
   def foreach_async[U](f: A => F[U]): F[Unit] = //...
 }


I.e., in the delayed variant, all original methods should collect operations into the next delayed object or perform an actual batched call.   
We also  have the method |finishChain|_,  which is called when we have no next call in the chain; an example of such a case is ``val x = c.withFilter(p)``.

By convention, the substituted type should be derived from trait |CallChainAsyncShiftSubst[F, T, FT]|_.


This structure has a nice categorical interpretation. If you are curious about that, read details in :ref:`categorical-interpretation-for-CallChainAsyncSubst`.

 
Builder methods.
^^^^^^^^^^^^^^^^

Yet one common usage pattern of high-order functions is builder methods, where we use high-order functions to build some processing algorithm.

.. code-block:: scala

 trait ReadChannel[F, A]:

   def map(f: A => B): ReadChannel[F, B]


Here, method ``map`` is used for building the streaming interface. We can provide an async variant of ``map`` which will return the same type as the original function:

.. code-block:: scala

 trait ReadChannel[F, A]:

   def map(f: A => B): ReadChannel[F, B]

   def mapAsync(f: A => F[B]): ReadChannel[F, B]


Also, we can see that our channel structure is already build on top of ``F[_]``, so it is not necessary to pass ``F`` to method parameter.
 
For convenience, |dotty-cps-async|_ supports both naming variants of ``mapAsync``: camelCase ``mapAsync`` and snake_case ``map_async``.

We propose to use the following convention when naming such methods:

- use ``method_async`` when the async method will unlikely be called directly by the programmer and will be used only for substitution in high-order function;
- use ``methodAsync`` when we expect that the developer can use this method directly along with cps substitution.


Async high-order functional interfaces  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For a case with an asynchronous high-order function interface (i.e. methods which accept functions like ``f:(A => F[B])``), the |async|_ macro can automatically transform the asynchronous result to have the same signature, so you can use |await|_ calls inside async lambdas without implementing additional methods or type classes.


.. ###########################################################################
.. ## Hyperlink definitions with text formatting (e.g. verbatim, bold)

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |CallChainAsyncShiftSubst[F, T, FT]| replace:: ``CallChainAsyncShiftSubst[F, T, FT]``
.. _CallChainAsyncShiftSubst[F, T, FT]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/runtime/CallChainAsyncShiftSubst.scala#L13

.. |CpsMonad[F]| replace:: ``CpsMonad[F]``
.. _CpsMonad[F]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L20

.. |finishChain| replace:: ``_finishChain``
.. _finishChain: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/runtime/IterableAsyncShift.scala#L427

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |DelayedWithFilter| replace:: ``DelayedWithFilter``
.. _DelayedWithFilter: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/runtime/IterableAsyncShift.scala#L420

.. |given AsyncShift[C]| replace:: ``given AsyncShift[C]``
.. _given AsyncShift[C]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/AsyncShift.scala#L11

.. |map| replace:: ``map``
.. _map: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#map[B](f:A=%3EB):List[B]

.. |Scala collections library| replace:: **Scala collections library**
.. _Scala collections library: https://www.scala-lang.org/api/current/scala/collection/index.html

.. |Scala standard library| replace:: **Scala standard library**
.. _Scala standard library: https://www.scala-lang.org/api/current/

.. |withFilter| replace:: ``withFilter``
.. _withFilter: https://www.scala-lang.org/api/current/scala/collection/immutable/List.html#withFilter(p:A=%3EBoolean):scala.collection.WithFilter[A,CC]
