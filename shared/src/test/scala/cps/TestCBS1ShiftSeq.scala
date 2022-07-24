package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure
//  TODO: check, while implicit resolution is changed
//import scala.collection._
import cps.testconfig.given


class TestBS1ShiftSeq:


  @Test def testDistinctBy(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val seq = Seq("1234","3452","1","12","21","777777777")
        seq.distinctBy{ x => await(T1.cbi(x.length)) }
     }
     c.run() match 
        case Success(s@Seq(a,b,c,d)) => assert( s.filter(_.length == 1).length == 1 )
                               assert( s.filter(_.length == 2).length == 1 )
                               assert( s.filter(_.length == 4).length == 1 )
        case _ => assert( "" == "shoud be seq from 4 elements") 


  @Test def testIndexWhereT(): Unit = 
     val c = async[ComputationBound]{
        val seq = Seq("1234","3452","1","12","21","777777777")
        seq.indexWhere{ x => await(T1.cbt(x.charAt(0)=='7')) }
     }
     assert(c.run()==Success(5))

  @Test def testIndexWhereF(): Unit = 
     val c = async[ComputationBound]{
        val seq = Seq("1234","3452","1","12","21","777777777")
        seq.indexWhere{ x => await(T1.cbt(x.charAt(0)=='8')) }
     }
     assert(c.run()==Success(-1))

/*
  // dotty bug: not compiled. TODO: debug and submit to  updtream
  class MyIndexedSeq[T](values: T*) extends IndexedSeq[T] 
                                        with scala.collection.IndexedSeqOps[T,MyIndexedSeq,MyIndexedSeq[T]]
                                         {

     val v: Vector[T] = Vector(values: _*)

     override def apply(i:Int): T = v(i)
     override def length: Int = v.length

     override def iterator: Iterator[T] = ???

  }
*/


  @Test def testIndexWhereIndexed(): Unit = 
     val c = async[ComputationBound]{
        val seq = IndexedSeq("1234","3452","1","12","21","777777777")
        seq.indexWhere{ x => await(T1.cbt(x.charAt(0)=='7')) }
     }
     assert(c.run()==Success(5))

  @Test def testSegmentLength(): Unit = 
     val c = async[ComputationBound]{
        val seq = Seq(1,2,3,4,5,6,7,8)
        seq.segmentLength( x => x < await(T1.cbi(6)) )
     }
     assert(c.run()==Success(5))

  @Test def testSegmentLengthIndexed(): Unit = 
     val c = async[ComputationBound]{
        val seq = IndexedSeq(1,2,3,4,5,6,7,8)
        seq.segmentLength( x => x < await(T1.cbi(6)) )
     }
     assert(c.run()==Success(5))


