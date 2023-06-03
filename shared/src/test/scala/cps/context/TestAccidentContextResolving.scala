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
import cps.testconfig.given

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
      //implicit val printCode = cps.macros.flags.PrintCode
      val c = async[Future] {
          val x = await(fun())
          x+1
      }
      FutureCompleter(c)

  class M1[X]
  class M2[X]
  
  given CpsMonad[M1] with CpsPureMonadInstanceContext[M1] with {
    def pure[A](x:A):M1[A] = M1[A]()
    def map[A,B](fa:M1[A])(f: A=>B):M1[B] = M1[B]()
    def flatMap[A,B](fa:M1[A])(f:A=>M1[B]):M1[B] = M1[B]()
  }

  given CpsMonad[M2] with CpsPureMonadInstanceContext[M2] with {
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

  @Test def twoSmallMonads2() = {
      val x = async[M2]{
        val y = await(m1)
        val z = await(m2)
      }
  }

  class ZM1[-R,+X]
      

  object ZM1:
    def access[R]: ZM1AccessPartialApply[R] =
      new ZM1AccessPartialApply[R]
    
  class ZM1AccessPartialApply[R]:
    def apply[A](f: R=>A):ZM1[R,A] = 
      ZM1[R,A]()
    
    

  class ZM2[-R,+X]

  class ZM1CpsMonad[R] extends CpsMonad[[X] =>> ZM1[R,X]] with CpsPureMonadInstanceContext[[X] =>> ZM1[R,X]] {
    def pure[A](x:A):ZM1[R,A] = ZM1[R,A]()
    def map[A,B](fa:ZM1[R,A])(f: A=>B):ZM1[R,B] = ZM1[R,B]()
    def flatMap[A,B](fa:ZM1[R,A])(f:A=>ZM1[R,B]):ZM1[R,B] = ZM1[R,B]()
  }

  class ZM2CpsMonad[R] extends CpsMonad[[X] =>> ZM2[R,X]] with CpsPureMonadInstanceContext[[X] =>> ZM2[R,X]] {
    def pure[A](x:A):ZM2[R,A] = ZM2[R,A]()
    def map[A,B](fa:ZM2[R,A])(f: A=>B):ZM2[R,B] = ZM2[R,B]()
    def flatMap[A,B](fa:ZM2[R,A])(f:A=>ZM2[R,B]):ZM2[R,B] = ZM2[R,B]()
  }

  given zm1Monad[R]: ZM1CpsMonad[R] = ZM1CpsMonad[R]()
  given zm2Monad[R]: ZM2CpsMonad[R] = ZM2CpsMonad[R]()
  
  given zm1Tpzm2[R1,R2]: CpsMonadConversion[[X]=>>ZM1[R1,X],[X]=>>ZM2[R2,X]] with {
    def apply[A](fa:ZM1[R1,A]):ZM2[R2,A] = ZM2[R2,A]()
  }

  given zm2Tpzm2[R1,R2]: CpsMonadConversion[[X]=>>ZM2[R1,X],[X]=>>ZM2[R2,X]] with {
    def apply[A](fa:ZM2[R1,A]):ZM2[R2,A] = ZM2[R2,A]()
  }


  def zm1:ZM1[Unit,Int] = ZM1[Unit,Int]()
  def zm2:ZM2[Any,Int] = ZM2[Any,Int]()


  @Test def twoSmallZMonads() = {
      class Apis {
        def futureApi: Future[Int] = Future successful 2
        def zm1Api[R]: ZM1[R,Int] = new ZM1[R,Int]()
      }
      val x = async[[X]=>>ZM2[Apis,X]]{
        val y = await(ZM1.access[Apis](_.futureApi))
        val z = await(zm2)
      }
  }
 
  
}
