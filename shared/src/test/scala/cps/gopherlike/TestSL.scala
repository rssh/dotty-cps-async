package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.testconfig.given

class TestSL:

  // should not be covariant
  class MyF[T]
    
  given CpsMonad[MyF] with CpsMonadInstanceContext[MyF] with
    def pure[A](a:A): MyF[A] = ???
    def map[A,B](fa:MyF[A])(f:A=>B):MyF[B] = ???
    def flatMap[A,B](fa:MyF[A])(f:A=>MyF[B]):MyF[B] = ???


  @Test def reproduce(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val writer = CIFWriter[MyF,Int]()
     val reader = CIFReader[MyF,Int](10)
     val sl = SLSelectLoop.create[MyF]
     sl.onReadAsync[Int](reader)(a => async{
          writer.write(a)
          true
     })
     // no assert, we interested in compilation

