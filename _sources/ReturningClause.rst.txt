Non-local returns
=================

You can use `returning <https://scala-lang.org/api/3.x/scala/util/control/NonLocalReturns$.html>`_ clause inside async block just like in plain Scala:

.. code-block:: scala

    def sourceWeights(item:Item, dataApi:DataApi):Future[A] = async[Future] {
       returning {
            val sources = await(dataApi.fetchSources(item))
            val weight = sources.foldLeft(0.0){ (weight,s) =>
                val si = await(dataApi.sourceInfo(s,item))
                if (si.isBlacklisted) {
                    throwReturn 0.0
                }
                weight + si.weight
            }
            weight
       }
    } 


Also, as in plain Scala, `throwReturn` does not intersect with handling `NonFatal` throwables:

.. code-block:: scala


    def validate(item:Item):Future[A] = async[Future] {
       returning {
            try
               if (!localCheck(item)) then
                   throwReturn false
               await(remoteCheck(item))
            catch
               case NonFatal(ex) =>
                 false
       }
    } 




Dotty-cps-async uses compile-time translation to avoid runtime overhead when this future is not used, so when using return
you should not hide catching `NonFatal` exceptions and `throwReturn` clauses from the `async` macro.

If you want to change the function name for `throwReturn`, be sure that your version is `transparent inline`:

.. code-block:: scala

  transparent inline def earlyReturn[T](t:T)(using ReturnThrowable[T]):Nothing =
    throwReturn(t)


Note, that non-local returns in Scala is deprecated.
Instead better to use `boundary/break <https://scala-lang.org/api/3.3.0/scala/util/boundary$.html>`_ introduced in Scala 3.3, which is supported by dotty-cps-async out of the box.


.. code-block:: scala

    def sourceWeights(item:Item, dataApi:DataApi):Future[A] = async[Future] {
       boundary {
            val sources = await(dataApi.fetchSources(item))
            val weight = sources.foldLeft(0.0){ (weight,s) =>
                val si = await(dataApi.sourceInfo(s,item))
                if (si.isBlacklisted) {
                    break(0.0)
                }
                weight + si.weight
            }
            weight
       }
    } 




