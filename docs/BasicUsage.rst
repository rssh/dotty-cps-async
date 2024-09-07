Dependency
==========

Sbt Example
-----------

The current prerelease is |0.9.22| for using with Scala |3.5.0|_.

Sbt dependency:

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.9.22"

JavaScript and Native targets are also supported.

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %%% "dotty-cps-async" % "0.9.22"


**Note**: :red:`%%%` automatically determines whether we are in a Scala/JVM or a Scala.js or a Scala.Native project (see |Scala.js Cross-Building|_).


If you use lts version of scala (i.e. scala-3.3.3) then you can use lts build of dotty-cps-async with name `dotty-cps-async-lts`:

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.9.22"






Compiler Plugin
---------------

For using direct context encoding (now marked as `@experimental`) you also need to add compiler-plugin:

for sbt:

 .. code-block:: scala

  autoCompilerPlugins := true
  addCompilerPlugin("com.github.rssh" %% "dotty-cps-async-compiler-plugin" % "0.9.22")

for mill:

 .. code-block:: scala

  def scalacPluginIvyDeps = Agg(ivy"com.github.rssh::dotty-cps-async-compiler-plugin:0.9.22")

Loom support on JVM
-------------------

If you use JDK-21 or later you can find helpful loom-based support for transforming arguments of high-order functions.
To enable one, add `dotty-cps-async-loom` module to the dependencies:

 .. code-block:: scala

   libraryDependencies += "com.github.rssh" %% "dotty-cps-async-loom" % "0.9.22"



Basic Usage
===========

Traditional async/await interface
---------------------------------


The usage is similar to working with async/await frameworks in Scala 2 (e.g. |scala-async|_) and in other languages.

We define two 'pseudo-functions' |async|_ and |await|_ [#f1]_ : 

 .. index:: async
 .. index:: await

 .. code-block:: scala

    def async[F[_], T](using am: CpsMonad[F])(expr: T) => F[T]

    def await[F[_], T](f: F[T])(using CpsMonad[F]): T



Inside the async block, we can use the |await|_ pseudo-function.


 .. code-block:: scala

    import cps._
    
    def myFun(params) = async[MyMonad] {
      // ... here is possible to use await: 
      val x = await(something) 
      // ...
    }


 .. index:: CpsMonad
 .. index:: CpsTryMonad

In the above code, the type ``MyMonad`` must implement one of the two type classes |CpsMonad|_ or |CpsTryMonad|_ (which supports try/catch).

The minimal complete snippet looks as follows:


 .. code-block:: scala

    package com.example.myModule

    import scala.concurrent.duration.DurationInt
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration
    import scala.util.{Failure, Success}
    import cps.*                  // async, await
    import cps.monads.{*, given}  // support for built-in monads (i.e. Future)

    object Example:

      def fetchGreeting(): Future[String] =  // dummy async function
        Future successful "Hi"

      def greet() = async[Future] {
        val greeting = await(fetchGreeting())
        println(greeting)
      }

      def main(args: Array[String]): Unit =
        val f = Await.ready(greet(), 1.seconds)
        f.failed.map { ex => println(ex.getMessage) }
  

This minimal example is for |Future|_ monad and depends on library |dotty-cps-async|_ to be added to our project file ``build.sbt`` :

 .. code-block:: scala

  // https://mvnrepository.com/artifact/com.github.rssh/dotty-cps-async
  libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.9.22"

From '0.9.22' on scala '3.5.0' or later,  we can use `await` as extension method:

 .. code-block:: scala

      def greet() = async[Future] {
        val greeting = fetchGreeting().await
        println(greeting)
      }


**Note**: The :ref:`Integrations` section lists further library dependencies needed for integration with well-known monadic frameworks such as |Cats Effect|_, |Monix|_, |ScalaZ IO|_ or |ZIO|_ and streaming frameworks like |Akka Streams|_ and |fs2|_. 


A monad  can also be abstracted out as in the following example:


 .. code-block:: scala

    trait Handler[F[_]: CpsTryMonad]:

      def run(): F[Unit] = async[F] {
        val connection = await(openConnection())
        try
          while
            val command = await(readCommand(connection))
            logCommand(command)
            val reply = await(handle(command))
            if !reply.isMuted then
              await(connection.send(reply.toBytes))
            !command.isShutdown
          do ()
        finally
          connection.close()

The |async|_ macro will transform the code block into something like

 .. raw:: html

  <details>
   <summary><a>transformed code</a></summary>

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

Since we use optimized monadic transform as the transformation technique, the number of monadic brackets will be  the
same as the number of |await|_ s in the source code.  
You can read the :ref:`notes about implementation details <random-notes>`.



Direct context encoding. (experimental)
---------------------------------------

Direct context encoding allows the representation of asynchronous API as ordinary synchronous calls using context parameter CpsDirect[F].
The signature above is an example of a function in direct encoding:


 .. code-block:: scala

   def  fetch(url:String)(using CpsDirect[Future]): String

Usage:

 .. code-block:: scala

   def fetchAccessible(urls:List[String])(using CpsDirect[Future]): Map[String,String] =
          urls.flatMap{ url =>
               try
                   Some((url, fetch(url)))
               catch
                   case NonFatal(ex) =>
                      logger.log(s"Can't fetch $url, skipping", ex)
                      None
          }.toMap


Our minimal example in this style:


 .. code-block:: scala

   import scala.annotation.experimental
   import scala.concurrent.*
   import scala.concurrent.duration.*
   import scala.concurrent.ExecutionContext.Implicits.global

   import cps.*                         //  import cps
   import cps.monads.{*,given}          //  import support for build-in monads (i.e. Future)


   @experimental
   class TestMinimalExample:

     def fetchGreeting()(using CpsDirect[Future]): String =    
       "Hi."  // assume this is a real async operation

     def greet()(using CpsDirect[Future]) = 
       val greeting = fetchGreeting()
       println(greeting)
 
     def main(args: Array[String]): Unit =
       val f = async[Future]{ greet() }
       Await.ready(f, Duration(1.seconds))
       f.failed.map { ex => println(ex.getMessage) }
  

I.e. function accept external context parameter of form `CpsDirect[F]` and return type is an ordinary value not wrapped in monad.
The developer can call such function from an async block or other function with the direct context.
Note, that signature also can be written in carried form: `def fetchGreeting(): CpsDirect[F] ?=> String`.

We can freely use `await` inside this direct context functions. Sometimes, we need to transform the synchronous style into asynchronous. We can do this using nested async expression or pseudo operator `asynchronized`  (reified with reify/reflect syntax), which uses current context for inferring the monad type. For example, here is a version of `fetchAccessibe` which fetch url-s in parallel:

 .. code-block:: scala

   def fetchAccessible(urls:List[String])(using CpsDirect[Future]): Map[String,String] =
          urls.map{ url => 
                 asynchronized(fetch(url))
               }
              .flatMap{ fetchingUrl =>
               try
                   Some((url, await(fetchingUrl)))
               catch
                   case NonFatal(ex) =>
                      logger.log(s"Can't fetch $url, skipping", ex)
          }.toMap


Note, that in current version (0.21) direct context encoding is marked to be experimental.


Alternative names
-----------------

`async(asynchronized)/await`  names is appropriate for Future-s and effect monads. There are other monads where a  direct style can be helpful
in applications such as probabilistic programming, navigation over search space, collections, and many other.
We define alternative names for macros: `reify(reifed)/reflect`, which can be more appropriate in the general case:


.. code-block:: scala

 def bayesianCoin(nFlips: Int): Distribution[Trial] = reify[Distribution] {
       val haveFairCoin = reflect(tf())
       val myCoin = if (haveFairCoin) coin else biasedCoin(0.9)
       val flips = reflect(myCoin.repeat(nFlips))
       Trial(haveFairCoin, flips)
  }


.. code-block:: scala

 import cps.*
 import cps.monads.{*,given}

 def allPairs[T](l: List[T]): List[(T,T)] = reify[List] {
       (reflect(l),reflect(l))
  }



Yet one pair of names 'lift/unlift', used for example in the |monadless|_ library by Flavio W. Brasill,  can be enabled by importing `cps.syntax.monadless.*`.


.. code-block:: scala

 import cps.*
 import cps.syntax.monadless.* 

 class TestMonadlessSyntax { 

  import cps.monads.FutureAsyncMonad

  val responseString: Future[String] = lift {
    try {
      responseToString(unlift(badRequest.get))
    } catch {
      case e: Exception => s"received an exceptional result: $e"
    }
  }

 }
 


.. rubric:: Footnotes

.. [#f1] The definitions of |async|_ and |await|_ are simplified, in reality they are more complex, because we want to infer the type of the expression independently from the type of monad.


.. ###########################################################################
.. ## Hyperlink definitions with text formating (e.g. verbatim, bold)

.. |0.9.19| replace:: ``0.9.19``
.. _0.9.19: https://repo1.maven.org/maven2/com/github/rssh/dotty-cps-async_3/0.9.19/

.. /*to update*/ 

.. |3.1.0| replace:: ``3.1.0``
.. _3.1.0: https://github.com/lampepfl/dotty/releases/tag/3.1.0

.. |3.1.1| replace:: ``3.1.1``
.. _3.1.1: https://github.com/lampepfl/dotty/releases/tag/3.1.1

.. |3.2.0| replace:: ``3.2.0``
.. _3.2.0: https://github.com/lampepfl/dotty/releases/tag/3.2.0

.. |3.3.1| replace:: ``3.3.0``
.. _3.3.1: https://github.com/lampepfl/dotty/releases/tag/3.3.1


.. |Akka Streams| replace:: **Akka Streams**
.. _Akka Streams: https://doc.akka.io/docs/akka/current/stream/

.. |fs2| replace:: **Fs2**
.. _fs2: https://fs2.io

.. |async| replace:: ``async``
.. _async: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L30

.. |await| replace:: ``await``
.. _await: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/Async.scala#L19

.. |Cats Effect| replace:: **Cats Effect**
.. _Cats Effect: https://typelevel.org/cats-effect/

.. |CpsMonad| replace:: ``CpsMonad``
.. _CpsMonad: https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala#L20

.. |CpsTryMonad| replace:: ``CpsTryMonad``
.. _CpsTryMonad: https://github.com/rssh/dotty-cps-async/blob/ff25b61f93e49a1ae39df248dbe4af980cd7f948/shared/src/main/scala/cps/CpsMonad.scala#L70

.. |dotty-cps-async| replace:: **dotty-cps-async**
.. _dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |Future| replace:: ``Future``
.. _Future: https://www.scala-lang.org/api/current/scala/concurrent/Future.html

.. |header_dotty-cps-async| replace:: dotty-cps-async
.. _header_dotty-cps-async: https://github.com/rssh/dotty-cps-async#dotty-cps-async

.. |header_scala3| replace:: Scala 3
.. _header_scala3: https://dotty.epfl.ch/

.. |Monix| replace:: **Monix**
.. _Monix: https://monix.io/

.. |monadless| replace:: ``monadless``
.. _monadless: https://github.com/monadless/monadless
.. |Scala 3| replace:: **Scala 3**
.. _Scala 3: https://dotty.epfl.ch/

.. |scala-async| replace:: ``scala-async``
.. _scala-async: https://github.com/scala/scala-async

.. |Scala.js Cross-Building| replace:: **Scala.js Cross-Building**
.. _Scala.js Cross-Building: https://www.scala-js.org/doc/project/cross-build.html

.. |ScalaZ IO| replace:: **ScalaZ IO**
.. _ScalaZ IO: https://scalaz.github.io

.. |ZIO| replace:: **ZIO**
.. _ZIO: https://zio.dev/
