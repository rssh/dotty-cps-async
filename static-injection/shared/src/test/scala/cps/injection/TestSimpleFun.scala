package cps.injection

import junit.*
import cps.injection.*
import junit.framework.TestCase
import org.junit.Test

class TestSimpleFun extends TestCase {

  @Test
  def testGivenInt(): Unit = {
    given Int = 2

    val t: ((Int => Boolean) => Boolean) = injectfull {
      inject[Int => Boolean](inject[Int])
    }
    println(t(_ % 2 == 0))

    def test[A](using A): Unit = injectfull {
      val a = inject[A]
      println(a)
    }

    print(test[Int])

  }


}