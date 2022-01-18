High-order functions.
=====================

|dotty-cps-async|_ supports the automatic transformation of high-order functions, where the lambda expression argument contains ``await``.  

For example, assume an HTTP client provides the following interface to fetch some data from a list of remote servers.

.. code-block:: scala

 trait HttpClient:
   def fetchData(url: String): Future[String] 


Then we can fetch data from all servers just by using ``await`` in the ``map`` argument:

.. code-block:: scala

     urls.map( await(httpClient.fetchData(_)) )


Note that the default ``map`` method will run all operations sequentially. Sequential evaluation is needed to allow the code to work correctly for a case of updating the multidimensional array in a ``for`` loop with asynchronous operations.

If we want all requests to run in parallel, we can start them in one map and, when all started - wait for the end of requests:

.. code-block:: scala

       urls.map( httpClient.fetchData(_) ).map(await(_))

During async transform, |dotty-cps-async|_ substitutes method ``map`` with signature ``List[A].map[B](f: A => B)`` to  

.. index:: AsyncShift

.. code-block:: scala

 summon[AsyncShift[List[A]]].map[F[_], B](c, summon[CpsMonad[F]])
                    

which is implemented in cps runtime with signature

.. code-block:: scala

   AsyncShift[List[A]].map[F[_], B](obj: List[A], cpsMonad: CpsMonad[F])(f: A => F[B])


|dotty-cps-async|_ includes implementations of shifted methods for most objects of the Scala standard library.

So, we can write something like

.. code-block:: scala

  val x = cache.getOrElse( await(fetchData() )


How to provide shifted functions.
---------------------------------


Functional interface.
^^^^^^^^^^^^^^^^^^^^^^

Suppose you want to make high-order methods of your class ``C`` be able to accept lambda functions with ``await``. 
For that purpose you have to implement the |given AsyncShift[C]|_ type class with a shifted version of your high-order methods.  
Such a 'shifted' version has an additional type parameter ``F[_]`` and an additional list of arguments, inserted first, containing the original object instance and an appropriate |CpsMonad[F]|_.  


Parameters should be changed in the following way:

* If the original parameter has type  ``A => B``, then changed: ``A => F[B]``
* If the original parameter is called by name with type ``=> A``, then changed: ``() => F[A]``
* Otherwise, the changed parameter has the same type as the original.


Example:

.. code-block:: scala

 case class TaggedValue[T](tag: String, value: T)
   def modified[S](f: T => S): TaggedValue[S] =
     TaggedValue(tag, f(x))

 class TaggedValueAsyncShift[T] extends AsyncShift[TaggedValue[T]]:
   def modified[F[_], S](o: TaggedValue[T], m: CpsMonad[F])(f: T => F[S]): F[TaggedValue[S]] =
     f(value).map(TaggedValue(tag,_))
             
 object TaggedValue:
   transparent inline given shiftedTaggedValue[T] as AsyncShift[TaggedValue[T] =
     TaggedValueAsyncShift[T]() 


Object-oriented interface.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In some cases, we use classes – defined in an object-oriented manner – with private data.  If we wants a class to provide an API for `dotty-cps-async <https://github.com/rssh/dotty-cps-async#dotty-cps-async>`_, then we can do this without breaking encapsulation. What is needed - to implement an async-shifted version of the function inside our class:

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


As we have seen, shifted functions have an additional type parameter: ``F[_]`` and a parameter |CpsMonad[F]|_ (or more specific type, if needed).  Async transformer will substitute the call of ``modify`` into ``modify_async`` during compilation.
   Sometimes, we already have ``F[_]`` as the type parameter of the enclosing class. We can omit those additional parameters in the async variant in such a case.

Note that you should carefully decide whether you need async function support and how to deal with concurrent modifications.  For example, in the code snippet below, different changes will interleave with each other.
 Usually, low-level constructs do not need async counterparts.


.. _substitutions-in-call-chains:

Special semantics for substitutions in call chains
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider a chain of calls, which accepts async-shifted functions.  One example is ``withFilter`` from the standard collections library.  Let's look at the following code:  

.. code-block:: scala

  for {
    url <- urls if await(status(url)) == Active
    items <- await(api.retrieveItems(url))
    Item <- items
  } yield item  


Here usual semantics of ``withFilter`` assume that we iterate over ``urls`` only once.  But if we translate this expression according to the standard rules, we will receive two passes: one pass in async ``withFilter`` and the second in ``flatMap``.

To perform the iteration once, we translate ``withFilter`` not to ``F[WithFilter]`` but to a substituted type ``DelayedWithFilter``, which holds the received predicate and delays actual evaluation upon the call of the next operation in the chain.

The implementation of class ``DelayedWithFilter`` looks like:

.. code-block:: scala

 class DelayedWithFilter[F[_], A, C[X] <: Iterable[X], CA <: C[A]](
     c: CA,
     m: CpsMonad[F],
     p: A => F[Boolean],
 ) extends CallChainAsyncSubst[F, WithFilter[A, C], F[WithFilter[A, C]] ]
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


I.e., in delayed variant, all original methods should or collect operations into the next delayed object or perform an actual batched call.   
Also, we have the method ``_finishChain``,  which is called when we have no next call in the chain; an example of such a case is ``val x = c.withFilter(p)``.  

By convention, the substituted type should be derived from ``CallChainAsyncSubst[F, T]``.


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


Also we can see that our channel structure is already build on top of ``F[_]``, so it is not necessary to pass ``F`` to method parameter.
 
For convenience `dotty-cps-async <https://github.com/rssh/dotty-cps-async#dotty-cps-async>`_ supports both naming variants of ``mapAsync``: camelCase ``mapAsync`` and snake_case ``map_async``.

We propose to use next convention when naming such methods:

- use ``method_async`` when the async method will unlikely be called directly by the programmer and will be used only for substitution in high-order function;
- use ``methodAsync`` when we expect that developer can use this method directly along with cps substitution.


Async high-order functional interfaces  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For a case with an asynchronous high-order function interface (i.e. methods which accept functions like ``f:(A => F[B])`` ), the ``async`` macro can automatically transform the asynchronous result to have the same signature, so you can use ``await`` calls inside async lambdas without implementing additional methods or type classes.


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |CpsMonad[F]| replace:: ``CpsMonad[F]``
.. _CpsMonad[F]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |given AsyncShift[C]| replace:: ``given AsyncShift[C]``
.. _given AsyncShift[C]: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/AsyncShift.scala
