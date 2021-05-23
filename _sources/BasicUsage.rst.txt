Dependency
===========

The current prerelease is 0.7.0 for using with scala3-3.0.0.

 .. code-block:: scala

   scalaVersion := "3.0.0"
   libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.8.1"

for JVM-projects. JavaScript also supported.

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "dotty-cps-async" % "0.8.1"



Basic Usage
===========

The usage is quite similar to working with async/await frameworks in Scala2 and other languages.
We have two 'pseudo-functions' ``async`` and ``await`` [#f1]_ : 

 .. index:: async
 .. index:: await

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


 .. index:: CpsMonad
 .. index:: CpsTryMonad

`MyMonad` should be a type for which we have implemented `CpsMonad <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala>`_ or `CpsTryMonad <https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L25>`_ (the latest supports try/catch) typeclass.


Monad can-be abstracted out as in next example:


 .. code-block:: scala

    trait Hanlder[F[_]: CpsTryMonad]:

      def run():F[Unit] = async[F]{
        val connection = await(openConnection())
        try
          while
            val command = await(readCommand(connection))
            logCommand(command)
            val reply = await(handle(command))
            if (!reply.isMuted)
               await(connection.send(reply.toBytes))
            !command.isShutdown
          do ()
        finally
          connection.close()

Async macro will transform code inside async to something like

 .. raw:: html

  <details>
   <summary><a>code</a></summary>

 .. code-block:: scala

   m.flatMap(openConnection())(a => {
     val connection: Connection[F] = a
     m.withAction({
       def _whilefun(): F[Unit] = 
         m.flatMap(
           m.flatMap(readCommand(connection))((a: Command) => {
             val command: Command = a
             logCommand(command)
             m.flatMap(handle(command))((a: Reply) => {
                val reply: Reply = a
                m.flatMap(
                  if (!reply.isMuted)
                    connection.send(reply.toBytes) 
                  else 
                     m.pure(())
                )( _ => m.pure(!command.isShutdown))
             })
           }))(c => if (c) _whilefun() else m.pure(()))
       _whilefun()
     })(
       m.pure(connection.close())
     )
   })

 .. raw:: html

  </details>

As transformation technique we use optimized monadic transform, the number of monadic brackets is the 
same as the numer of ``await`` s in code.  
You can read the :ref:`notes about implementation details <random-notes>`.


.. rubric:: Footnotes

.. [#f1]  the definitions are simplified, in reality they are more complex, because we want infer the type of expression independently from the type of monad.
 

