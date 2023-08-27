package cpstest

import cps.*
import cps.monads.{*,given}
import scala.concurrent.*



class TestShiftedDirectM1 {

  def  dmap[A,B](s: Seq[A])(f: A=>B)(using CpsDirect[Future]): Seq[B] = {
      s.map(f)
  }

  def  dmapAsync[A,B](s: Seq[A])(f: A=>Future[B])(using CpsDirect[Future]): Future[Seq[B]] = {
      s.map(f).sequence
  }

  def one(using CpsDirect[Future]): Int = 1


  def testMap(using ec: ExecutionContext): Unit =
     async[Future]{
        val r = dmap(1 to 10)(x => x+one)
        assert(r.sum == 65)
     }



}