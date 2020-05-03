package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure


class TestBS1ShiftCollectionOps:


  @Test def testMapList(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        List(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(List(4,5,6)))

  @Test def testMapSeq(): Unit = 
     val c = async[ComputationBound]{
        Seq(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(Seq(4,5,6)))

  @Test def testForeachSeq(): Unit = 
     val c = async[ComputationBound]{
        var s = 0
        Seq(1,2,3).foreach{ x =>
           s += await(T1.cbi(3)) + x
        }
        s
     }
     assert(c.run() == Success(15))


  @Test def testMapSet(): Unit = 
     val c = async[ComputationBound]{
        Set(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     assert(c.run() == Success(Set(4,5,6)))
  
  @Test def testCountList(): Unit =
     val c = async[ComputationBound]{
               List(T1.cbi(1),T1.cbi(2),T1.cbi(3)).count(x => await(x)%2==1)
             }
     assert(c.run() == Success(2))

  @Test def testDropWhileVector(): Unit =
     val c = async[ComputationBound]{
               Vector(T1.cbi(1),T1.cbi(2),T1.cbi(3)).dropWhile(x => await(x) < 3)
             }
     c.run() match {
        case Success(v) =>
            assert(v(0).run() == Success(3))
        case Failure(ex) =>
            assert(false,"dropWhile result should be successed")
     }

  @Test def testExistsVectorT(): Unit =
     val c = async[ComputationBound]{
                Vector(T1.cbi(1),T1.cbi(2),T1.cbi(3)).exists(x => await(x)==2)
     }
     assert(c.run() == Success(true))

  @Test def testExistsVectorF(): Unit =
     val c = async[ComputationBound]{
                Vector(T1.cbi(1),T1.cbi(2),T1.cbi(3)).exists(x => await(x)==1000)
     }
     assert(c.run() == Success(false))

  @Test def testFindListT(): Unit =
     val c = async[ComputationBound]{
                List(T1.cbi(1),T1.cbi(2),T1.cbi(3)).find{x => await(x)==3 }
     }
     val l = c.run()
     l match {
        case Success(Some(v)) =>
            assert(v.run() == Success(3))
        case Success(None) =>
            assert(false,"find result should be not None")
        case Failure(ex) =>
            assert(false,"find result should be successed")
     }

  @Test def testFindListF(): Unit =
     val c = async[ComputationBound]{
                List(T1.cbi(1),T1.cbi(2),T1.cbi(3)).find( x => await(x)==1000 )
     }
     val l = c.run()
     assert(l == Success(None)) 

  @Test def testFoldSet1(): Unit =
     val c = async[ComputationBound]{
          Set(1,2,3).fold(0)(
                   (x:Int,y:Int) => x + y + await(T1.cbi(1)) 
          )
     }
     val l = c.run()
     assert(l == Success(9)) 

/*
  @Test def testFoldSet2(): Unit =
     val c = async[ComputationBound]{
          Set(T1.cbi(1),T1.cbi(2),T1.cbi(3)).fold(T1.cbi(4))(
                   (x,y) => CpsMonad[ComputationBound].map(x)(_ + await(y)) )
     }
     val l = c.run()
     assert(l == Success(10)) 
*/

