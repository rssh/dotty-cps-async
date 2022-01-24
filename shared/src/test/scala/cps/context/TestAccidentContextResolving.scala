package cps.context

import scala.util.*
import scala.util.control.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

import cps.*
import cps.monads.{given,*}
import cps.util.FutureCompleter
import cps.ComputationBoundAsyncMonad

import org.junit.{Test,Ignore}
import org.junit.Assert._

class TestAccidentContextResolving {

  // let-s define two cps instance monads in scope,
  //  to try fail implicit resolving of await implicit params

  def fun():Future[Int] = {
    Future successful 1
  }
  
  @Test def testCBAwaitInScopwWithTwoInstanceContext() = 
      //implicit val printCode = cps.macros.flags.PrintCode
      val c = async[ComputationBound] {
         val x = await(fun())
         x+1
      }
      val fr = c.runTicks(1.second)
      val r = fr.map(x => assert(x == 2)) 
      FutureCompleter(r)

  @Test def testFtAwaitInScopwWithTwoInstanceContext() = 
      implicit val printCode = cps.macros.flags.PrintCode
      val c = async[Future] {
          val x = await(fun())
          x+1
      }
      FutureCompleter(c)

  class M1[X]
  class M2[X]
  
  given CpsMonad[M1] with CpsMonadInstanceContext[M1] with {
    def pure[A](x:A):M1[A] = M1[A]()
    def map[A,B](fa:M1[A])(f: A=>B):M1[B] = M1[B]()
    def flatMap[A,B](fa:M1[A])(f:A=>M1[B]):M1[B] = M1[B]()
  }

  given CpsMonad[M2] with CpsMonadInstanceContext[M2] with {
    def pure[A](x:A):M2[A] = M2[A]()
    def map[A,B](fa:M2[A])(f: A=>B):M2[B] = M2[B]()
    def flatMap[A,B](fa:M2[A])(f:A=>M2[B]):M2[B] = M2[B]()
  }

  given CpsMonadConversion[M1,M2] with {
    def apply[A](fa:M1[A]):M2[A] = M2[A]()
  }

  given CpsMonadConversion[M2,M1] with {
    def apply[A](fa:M2[A]):M1[A] = M1[A]()
  }

  def m1:M1[Int] = M1[Int]()
  def m2:M2[Int] = M2[Int]()

  @Test def twoSmallMonads() = {
      val x = async[M1]{
        val y = await(m1)
        val z = await(m2)
      }
  }    

}
