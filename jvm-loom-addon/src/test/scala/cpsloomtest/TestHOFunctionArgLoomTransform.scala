package cpsloomtest

import cps.*
import cps.monads.{*,given}
import cps.loom.{*,given}

import scala.concurrent.*
import scala.concurrent.duration.*
import org.junit.{Test,Ignore}


sealed trait MyList[+A] {

  def map[B](f: A=>B): MyList[B]

  def length: Int

}

case object MyNil extends MyList[Nothing] {

  override def map[B](f: Nothing => B): MyList[B] = MyNil

  override def length: Int = 0

}

case class MyCons[T](hd: T, tl: MyList[T]) extends MyList[T] {

    override def map[B](f: T => B): MyList[B] = MyCons(f(hd), tl.map(f))

    override def length: Int = 1 + tl.length

}

object MyList {

  def create[T](args: T*):MyList[T] = {
    if (args.isEmpty) MyNil
    else MyCons(args.head, create(args.tail*))
  }

}


class TestHOFunctionArgLoomTransform {

  import scala.concurrent.ExecutionContext.Implicits.global

  def fetch(url:String):Future[String] =
    Future successful s"fetched(${url})"

  @Test
  def testAwaitInMap() = {
    val f = async[Future] {
      val list = MyList.create("http://example1.com", "http://example2.com", "http://example3.org")
      val fetched = list.map{ url =>
        await(fetch(url))
      }
      assert(fetched.length == 3)
    }
    Await.result(f, 1.second)
  }


}
