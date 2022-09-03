package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure

import cps.testconfig.given


class TestBS1ShiftIterableOps:


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

  @Test def testMapSeqVal(): Unit = 
     val c = async[ComputationBound]{
        val seq = Seq(1,2,3)
        seq.map{ x =>
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

  @Test def testForeachRange(): Unit = 
     val c = async[ComputationBound]{
        var s = 0
        for(i <- 1 to 3) {
           s += await(T1.cbi(3)) + i
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

  @Test def testFoldSet2(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          Set(T1.cbi(1),T1.cbi(2),T1.cbi(3)).fold(T1.cbi(4))(
                         (x,y) => async(await(x) + await(y))  )
     }
     val l = c.run()
     val l1 = l.get.run()
     assert(l1 == Success(10)) 


  @Test def testCollectFind(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c = List(1,2,3,4)
          val r: Option[Int] = c.collectFirst{ case x if x > 2 => x + await(T1.cbi(1)) }
          //val r: Option[Int] = await(T1.cbt(Some(4)))
          r
     }
     assert(c.run() == Success(Some(4)))

  @Test def testCollect(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c = List(1,2,3,4)
          c.collect{ case x if x % 2 == 0 => x + await(T1.cbi(1)) }
     }
     assert(c.run() == Success(List(3,5)))


  @Test def testGroupByEmpty(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:List[String] = List()
          c.groupBy(x => await(T1.cbi(x.length)))
     }
     assert(c.run() == Success(Map.empty[Int,String]))

  @Test def testGroupBy(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:List[String] = List("","a","aa","bb","aaa","bbb","ccc")
          c.groupBy(x => await(T1.cbi(x.length)))
     }
     val r = c.run().get
     assert(r(0).toList.size == 1)
     assert(r(1).toList.size == 1)
     assert(r(2).toList.size == 2)
     assert(r(3).toList.size == 3)


  @Test def testCorrespondsSame(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
         val l1 = List(1,2,3)
         val l2 = l1
         l1.corresponds(l2)((a,b) => await(T1.cbt(a == b)) )
     }
     assert(c.run() == Success(true))

  @Test def testCorrespondsNoSame(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
         val l1 = List(1,2,3)
         val l2 = List(1,4,1)
         l1.corresponds(l2)((a,b) => await(T1.cbt(a == b)) )
     }
     assert(c.run() == Success(false))

  @Test def testCorrespondsDiffLen1(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
         val l1 = List(1,2,3)
         val l2 = List(1,2,3,3)
         l1.corresponds(l2)((a,b) => await(T1.cbt(a == b)) )
     }
     assert(c.run() == Success(false))

  @Test def testCorrespondsDiffLen2(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
         val l1 = List(1,2,3,3)
         val l2 = List(1,2,3)
         l1.corresponds(l2)((a,b) => await(T1.cbt(a == b)) )
     }
     assert(c.run() == Success(false))


  @Test def testFoldLeft(): Unit =
     val c = async[ComputationBound]{
          Seq(1,2,3).foldLeft("")((s,e) => "(" + s + "," + await(T1.cbi(e)).toString + ")" )
     }
     assert(c.run() == Success("(((,1),2),3)"))

  @Test def testFoldRight(): Unit =
     val c = async[ComputationBound]{
          Seq(1,2,3).foldRight("")((e,s) => "(" + await(T1.cbs(e.toString)) + "," + s + ")" )
     }
     assert(c.run() == Success("(1,(2,(3,)))"))


  @Test def forall1(): Unit =
     val c = async[ComputationBound]{
          Seq(1,2,3).forall(_ == await(T1.cbi(3)))
     }
     assert(c.run() == Success(false))

  @Test def forall2(): Unit =
     val c = async[ComputationBound]{
          Seq(1,2,3).forall(_ > await(T1.cbi(0)))
     }
     assert(c.run() == Success(true))

  @Test def forall3(): Unit =
     val c = async[ComputationBound]{
          Seq().forall(_ == await(T1.cbi(0)))
     }
     assert(c.run() == Success(true))

  @Test def testGroupMap1(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:List[String] = List("","a","aa","bb","aaa","bbb","ccc")
          c.groupMap(x => await(T1.cbi(x.length)))(identity)
     }
     val r = c.run().get
     assert(r(0).toList.size == 1)
     assert(r(1).toList.size == 1)
     assert(r(2).toList.size == 2)
     assert(r(3).toList.size == 3)

  @Test def testGroupMap2(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:List[String] = List("","a","aa","bb","aaa","bbb","ccc")
          c.groupMap(x => await(T1.cbi(x.length)))(x => await(T1.cbs(x+"1")))
     }
     val r = c.run().get
     assert(r(0).toList.size == 1)
     assert(r(0)(0) == "1")
     assert(r(1).toList.size == 1)
     assert(r(1)(0) == "a1")
     assert(r(2).toList.size == 2)
     assert(r(3).toList.size == 3)


  @Test def testGroupMapReduce(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
          val c:List[String] = List("","a","aa","bb","aaa","bbb","ccc")
          c.groupMapReduce(x => await(T1.cbi(x.length)))(x => await(T1.cbs(x+"1")))( _ + _ )
     }
     val r = c.run().get
     assert(r(0) == "1")
     assert(r(1) == "a1")
     assert(r(2) == "aa1bb1")
     assert(r(3) == "aaa1bbb1ccc1")

  @Test def testGroupMapReduce1(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async[ComputationBound]{
          val c:List[String] = List("","a","aa","bb","aaa","bbb","ccc")
          c.groupMapReduce(x => await(T1.cbi(x.length)))(x => await(T1.cbs(x+"1")))( (w:String,v:String) => w + await(T1.cbs(v)) )
     }
     val r = c.run().get
     assert(r(0) == "1")
     assert(r(1) == "a1")
     assert(r(2) == "aa1bb1")
     assert(r(3) == "aaa1bbb1ccc1")

  @Test def testMaxBy(): Unit =
     val c = async[ComputationBound]{
          val x: Seq[Int] = (1 to 100).toSeq 
          x maxBy ( _ % await(T1.cbi(77)) )
     }
     val r = c.run().get
     assert(r == 76)

  @Test def testMinBy(): Unit =
     val c = async[ComputationBound]{
          val x: Seq[Int] = (1 to 100).toSeq 
          x minBy ( _ % await(T1.cbi(77)) )
     }
     val r = c.run().get
     assert(r == 77)

  @Test def testScanLeft(): Unit =
     val c = async[ComputationBound]{
          val l = (1 to 5).toSeq
          l.scanLeft(0)( (x,y) => x + await(T1.cbi(y)) )
     }
     val r = c.run().get
     assert(r == Seq(0,1,3,6,10,15))

  @Test def testScanRight(): Unit =
     val c = async[ComputationBound]{
         val l = (1 to 5).toSeq
         l.scanRight(0){ (x,y) => 
            x + await(T1.cbi(y)) 
         }
     }
     val checkC = { 
       val l = (1 to 5).toSeq
       l.scanRight(0){ (x,y) =>
         x + y
       }
     }
     val r = c.run().get
     assert(r == checkC)
     assert(r == Seq(15,14,12,9,5,0))


  @Test def testTakeWhile(): Unit =
     val c = async[ComputationBound]{
        val l = Vector(1,2,3,4,5,6,7,8,3,1)
        l.takeWhile( _ < await(T1.cbi(5)) )
     }
     val r = c.run().get
     assert(r == Vector(1,2,3,4))

  @Test def testSpan(): Unit =
     val c = async[ComputationBound]{
        val l = Vector(1,2,3,4,5,6,7,8,3,1)
        l.span( _ < await(T1.cbi(5)) )
     }
     val r = c.run().get
     assert(r._1 == Vector(1,2,3,4))
     assert(r._2 == Vector(5,6,7,8,3,1))

  @Test def testPartition(): Unit =
     val c = async[ComputationBound]{
        val l = Vector(1,2,3,4,5,6,7,8,3,1)
        l.partition( _ < await(T1.cbi(5)) )
     }
     val r = c.run().get
     assert(r._1 == Vector(1,2,3,4,3,1))
     assert(r._2 == Vector(5,6,7,8))

  @Test def testPartitionMap(): Unit =
     val c = async[ComputationBound]{
        val l = Vector(1,2,3,4,5,6,7,8,3,1)
        l.partitionMap( x => if (x < await(T1.cbi(5))) Left(x) else Right(x.toString) )
     }
     val r = c.run().get
     assert(r._1 == Vector(1,2,3,4,3,1))
     assert(r._2 == Vector("5","6","7","8"))

  @Test def testReduce1(): Unit =
     val c = async[ComputationBound]{
        val l = 1 to 10
        l.reduce((x,y) => await(T1.cbi(x+y)))
     }
     val r = c.run().get
     assert(r == (1 to 10).sum )

  @Test def testReduceEmpty(): Unit =
     val c = async[ComputationBound]{
        val l = List.empty[Int]
        l.reduce((x,y) => await(T1.cbi(x+y)))
     }
     assert( c.run().isFailure )

  @Test def testReduceLeft(): Unit =
     val c = async[ComputationBound]{
        val l = (0 to 9).map(_.toString)
        l.reduceLeft((x,y) => await(T1.cbs("("+x+y+")")))
     }
     val r = c.run().get
     assert(r == "(((((((((01)2)3)4)5)6)7)8)9)" )

  @Test def testReduceLeftEmpty(): Unit =
     val c = async[ComputationBound]{
        val l = List.empty[String]
        l.reduceLeft((x,y) => await(T1.cbs("("+x+y+")")))
     }
     val r = c.run()
     assert(r.isFailure)

  @Test def testReduceRight(): Unit =
     val c = async[ComputationBound]{
        val l = (0 to 9).map(_.toString)
        l.reduceRight((x,y) => await(T1.cbs("("+x+y+")")))
     }
     val r = c.run().get
     assert(r == "(0(1(2(3(4(5(6(7(89)))))))))" )

  @Test def testReduceRightEmpty(): Unit =
     val c = async[ComputationBound]{
        val l = List.empty[String]
        l.reduceRight((x,y) => await(T1.cbs("("+x+y+")")))
     }
     val r = c.run()
     assert(r.isFailure)

  @Test def testTapEach(): Unit =
     class X(val value:Int, var tapped:Boolean)
     val v = (1 to 10).map( new X(_, false) )
     val c = async[ComputationBound]{
        v.tapEach( x => { await(T1.cbi(0)); x.tapped = true  })
     }
     val r = c.run()
     assert(v.forall(_.tapped))

  @Test def testWithFilterSmoke(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        (0 to 9).withFilter(x => await(T1.cbi(0)) == x%2).map(_.toString) 
     }
     val r = c.run()
     assert(r == Success(IndexedSeq("0","2","4","6","8")) )



