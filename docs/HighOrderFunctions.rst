High-order functions.
=====================

Dotty-cps-async supports the automatic transformation of high-order functions,  where the lambda expression argument contains ``await``.  

Example -- let us have a list of remote servers and want to fetch some data from each of them. 
Assume, that out http client provides next interface:

.. code-block:: scala

 trait HttpClient:
    def fetchData(url: String): Future[String] 


Then we can fetch data from all servers just by using ``await`` in the ``map`` argument:

.. code-block:: scala

     urls.map( await(httpClient.fetchData(_)) )


Note that the default ``map`` will run all operations sequentially. Sequential order of evaluation is needed to allow the code, like updating the multidimensional array in for loop, works correctly in an asynchronous case.

If we want all requests to run in parallel, we can start them in one map and when all started - wait for the end of requests:

.. code-block:: scala

       urls.map( httpClient.fetchData(_) ).map(await(_))

During async transform, dotty-cps-async substitute method map of you List with signature  
   ``List[A].map[B](f: A=>B)`` to  

:: index:: AsyncShift

.. code-block:: scala

 summon[AsyncShift[List[A]]].map[F[_],B](c,summon[CpsMonad[F]])
                    

which is implemented in cps runtime with signature

.. code-block:: scala

   AsyncShift[List[A]].map[F[_],B](obj:List[A],cpsMonad:CpsMonad[F])(f: A=> F[B])


Dotty-cps-async includes implementations of shifted methods for most of the standard library objects. So, it is possible to write something like ``x=cache.getOrElse( await(fetchData() )`` .


Providing shifted functions.
----------------------------


Functional interface.
^^^^^^^^^^^^^^^^^^^^^^

Suppose you want to make high-order methods of your class ``C`` be able to accept lambda functions with await. 
In that case, you should implement ``given AsynsShift[C]`` typeclass with a shifted version of your high-order methods.  
Such a 'shifted' version has an additional type parameter: ``F[_]``  and an additional list of arguments, inserted first, which contains the original object instance and an appropriative ``CpsMonad[F]``.  


Parameters should be changed in the following way:

* If the origin parameter has type  ``A=>B``, then changed: ``A => F[B]``
* If the origin parameter is called by name with type ``=>A``, then changed: ``()=>F[A]``
* Otherwise, the changed parameter has the same type as the origin.


Example:

.. code-block:: scala

 case class TaggedValue[T](tag: String, value:  T)
      def   modified[S](f: T => S): TaggedValue[S] =
          TaggedValue(tag, f(x))

 class TaggedValueAsyncShift[T] extends AsyncShift[TaggedValue[T]]:

      def modified[F[_],S](o:TaggedValue[T], m: CpsMonad[F])(f: T=>F[S]): F[TaggedValue[S]] =
          f(value).map(TaggedValue(tag,_))
             
 object TaggedValue:

      transparent inline given shiftedTaggedValue[T] as AsyncShift[TaggedValue[T] =
                                                                               TaggedValueAsyncShift[T]() 


Object oriented interface (obsolete, up to 0.3.5)
^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes, we can use classes, defines in an object-oriented manner, where data is private inside class.  If the developer of such a class wants to provide API for dotty-cps-async, then he/she can do this without breaking encapsulation. What is needed - to implement AsyncShifted[F:CpsMonad] version inside  you class, which will accept methods with shifted parameters, and made a given ObjectAsync which should create instance of AsyncShifted from object and CpsMonad.

Example:

.. code-block:: scala

 class  MyIntController:
    private var x:  Int = 0;

    def  modify(f: Int => Int): Int =
       val old = x
       x = f(x)
       sendSignal(x)
       old

    def modify_async[F[_]](m: CpsMonad[M])(f: Int => F[Int]): F[Int] =
       val old = x
       m.map(f(x))(_ => { sendSignal(x); old }) 


.. code-block:: scala


Note that you should carefully decide whether you need async function support and how to deal with concurrent modifications.  For example, in the code snippet below, different changes will interleave with each other.
 Usually, low-level constructs do not need async counterparts.


Object oriented interface (after 0.3.5)
^^^^^^^^^^^^^^^^^^^^^^^^^^






