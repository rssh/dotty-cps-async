package cpstest

import cps.*
import cps.monads.{*,given}
import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global


@experimental
class TestShiftedDirectM1 {

  def  dmap[A,B](s: Seq[A])(f: A=>B)(using CpsDirect[Future]): Seq[B] = {
      s.map(f)
  }

  def  dmapAsync[A,B](s: Seq[A])(f: A=>Future[B])(using cpsDirect: CpsDirect[Future]): Seq[B] = {
      await(Future.sequence(s.map(f)))
  }

  def one(using CpsDirect[Future]): Int = 1


  def testMap(using ec: ExecutionContext): Future[Unit] =
     async[Future]{
        val r = dmap(1 to 10)(x => x+one)
        assert(r.sum == 65)
     }



}

@experimental
object TestShiftedDirectM1 extends TestShiftedDirectM1 {

  def main(args:Array[String]): Unit = {
      val f = testMap(using ExecutionContext.global)
      Await.result(f, 10.seconds)
      println("Ok")
  }

}