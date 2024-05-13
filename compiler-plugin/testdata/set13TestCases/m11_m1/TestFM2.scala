package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.concurrent.Future
import scala.util.Success

import cps.*
import cps.plugin.annotation.*

import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin


//@CpsDebugLevel(20)
class TestFM2:

  trait IFReader1[F[_]:CpsSchedulingMonad,A]{

     def aOptRead() : F[Option[A]] = ???

     transparent inline def optRead()(using monadContext: CpsMonadContext[F]): Option[A] =
       await(aOptRead())

  }


  class FlatMapped[F[_]:CpsSchedulingMonad,A,B](f: A=>IFReader1[F,B])  {


     def prevOptRead()(using monadContext: CpsMonadContext[F]): Option[A] =
       ???

     def run(): F[Unit] = 
      //implicit val printCode = cps.macroFlags.PrintCode
      //implicit val printTree = cps.macroFlags.PrintTree
      //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
      async[F]{
          prevOptRead() match
            case Some(a) =>
              val internal = f(a)
              internal.optRead().isDefined
            case None =>
              false
     }

   }


  



