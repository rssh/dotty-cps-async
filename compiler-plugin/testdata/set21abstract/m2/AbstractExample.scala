package m1

import scala.annotation.experimental
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import cps.*
import cps.monads.{*, given}


/**
 * The same as AbstractExample, but in carried form (ie. using CpsDirect in return type)
 */
@experimental
trait AbstractExample {

  def method(x:Int): (CpsDirect[Future]) ?=> Int

}

@experimental
class Example1 extends AbstractExample {

  def method(x:Int):  CpsDirect[Future]) ?=> Int = {
    x + 1
  }

}

@experimental
class Example2 extends AbstractExample {

  def method(x:Int) CpsDirect[Future] ?=> Int = {
    val y = await(Future.successful(x+1))
    y + 1
  }

}

@experimental
object Main {

  def main(args:Array[String]):Unit = {
    val example1 = Example1()
    val example2 = Example2()
    val fv = async[Future] {
      val r1 = example1.method(1)
      val r2 = example2.method(1)
      (r1, r2)
    }
    val (r1, r2) = Await.result(fv, 1.second)
    if (r1 == 2 && r2 == 3) {
      println("Ok")
    } else {
      println(s"Error: $r1, $r2")
    }
  }

}