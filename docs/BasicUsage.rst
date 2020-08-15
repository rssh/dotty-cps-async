Basic Usage
===========

Usage is obvious: we have two 'pseudo-functions' ``async`` and ``await`` [#f1]_ : 


 .. code-block:: scala

    def async[F[_], T](using am:CpsMonad[F]):(expr: T)=>F[T]

    def  await[F[_], T](f: F[T])(using CpsMonad[F]): T



Inside the async block, we can use await pseudo-function.


 .. code-block:: scala

    import cps._
    
    def  myFun(params) = async[MyMonad] {
         // ... here is possible to use await: 
         val x = await(something) 
         // ...
    }


`MyMonad` should be a type for which we have implemented `CpsMonad <https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/CpsMonad.scala>`_ or `CpsTryMonad <https://github.com/rssh/dotty-cps-async/blob/master/src/main/scala/cps/CpsMonad.scala#L25>`_ (the latest supports try/catch) typeclass.


Monad can-be abstracted out as in next example:::


 .. code-block:: scala

    trait Hanlder[F[_]: CpsTryMonad]:

      def run():F[Unit] = async[F]{
        val connection = await(openConnection())
        try
          while
            val command = await(readCommand(connection))
            val reply = await(handle(command))
            if (!reply.isMuted)
               await(connection.send(reply.toBytes))
            !command.isShutdown
          do ()
        finally
          connection.close()



.. rubric:: Footnotes

.. [#f1]  the definitions are simplified, in reality they are more complex, because we want infer the type of expression independently from the type of monad.
 
