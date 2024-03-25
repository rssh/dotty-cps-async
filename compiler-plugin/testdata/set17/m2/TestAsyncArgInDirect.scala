package cpstest


class TestAsyncArgInDirect {

  def foo(x: Int)(using CpsDirect[Future]): Int = x + 1

  def one(using CpsDirect[Future]): Int = 1

  def testAsyncArgInDirect() = {
    val f = async[Future] {
      val x = foo(one)
      x
    }
    val r = Await.ready(f)
    assert(r==2)
  }



}