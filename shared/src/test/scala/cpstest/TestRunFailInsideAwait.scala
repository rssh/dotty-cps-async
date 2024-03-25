package cpstest;

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.junit.Test

import cps.*
import cps.monads.{*,given}

import cps.testconfig.given
import cps.util.FutureCompleter


class TestRunFailInsideAwait {

    @Test
    def testFailInsideAwait()  = {
        val c = async[Future] {
            val list0 = List(1, 2, 3, 4, 5)
            try {
                val list1 = list0.map[Int](x => await(Future.failed(new RuntimeException("test"))))
                assert(false)
                1
            } catch {
                case ex: RuntimeException =>
                    assert(ex.getMessage() == "test")
                    2
            }
        }
        FutureCompleter(c.map(x => assert(x == 2)))
    }

}
