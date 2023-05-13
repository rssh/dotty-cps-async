package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure

import cps.testconfig.given


class TestCBS1ShiftArrayOps:


  @Test def testMap(): Unit = 
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async[ComputationBound]{
        Array(1,2,3).map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     val r = c.run()
     val v = r.get
     assert(v(0)==4)
     assert(v(1)==5)
     assert(v(2)==6)

  @Test def testMapVal(): Unit = 
     val c = async[ComputationBound]{
        val arr = Array(1,2,3)
        arr.map{ x =>
           await(T1.cbi(3)) + x
        }
     }
     val r = c.run()
     val v = r.get
     assert(v(0)==4)
     assert(v(1)==5)
     assert(v(2)==6)

  //mapInPlace: impossible (we have no access to setters)

  @Test def testForeachArray(): Unit = 
     val c = async[ComputationBound]{
        var s = 0
        Array(1,2,3).foreach{ x =>
           s += await(T1.cbi(3)) + x
        }
        s
     }
     assert(c.run() == Success(15))

  
  @Test def testCount(): Unit =
     val c = async[ComputationBound]{
               Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).count(x => await(x)%2==1)
             }
     assert(c.run() == Success(2))

  @Test def testDistinctBy(): Unit = {
     val c = async[ComputationBound]{
        Array("a","bbb","c","dd","ee","rrr","rrrr","12345678","1").distinctBy( x => await(T1.cbi(x.length)) )
     }
     val r = c.run().get
     assert(r(0)=="a")
     assert(r(1)=="bbb")
     assert(r(2)=="dd")
     assert(r(3)=="rrrr")
  }

  @Test def testDropWhile(): Unit =
     val c = async[ComputationBound]{
               Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).dropWhile(x => await(x) < 3)
             }
     c.run() match {
        case Success(v) =>
            assert(v(0).run() == Success(3))
        case Failure(ex) =>
            assert(false,"dropWhile result should be successed")
     }

  @Test def testExistsT(): Unit =
     val c = async[ComputationBound]{
                Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).exists(x => await(x)==2)
     }
     assert(c.run() == Success(true))

  @Test def testExistsF(): Unit =
     val c = async[ComputationBound]{
                Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).exists(x => await(x)==1000)
     }
     assert(c.run() == Success(false))

  @Test def testFindT(): Unit =
     val c = async[ComputationBound]{
                Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).find{x => await(x)==3 }
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

  @Test def testFindF(): Unit =
     val c = async[ComputationBound]{
                Array(T1.cbi(1),T1.cbi(2),T1.cbi(3)).find( x => await(x)==1000 )
     }
     val l = c.run()
     assert(l == Success(None)) 

  @Test def testFilter(): Unit =
     val c = async[ComputationBound]{
              Array(1,2,3,4,5,6,7,8,9,10).filter( x => x % 3 == await(T1.cbi(0)) )
     }
     val r = c.run().get
     assert(r(0)==3)
     assert(r(1)==6)
     assert(r(2)==9)
     

  @Test def testFlatMap(): Unit =
      //implicit val printCode = cps.macros.flags.PrintCode
      //implicit val printTree = cps.macros.flags.PrintTree
      val c = async[ComputationBound]{
         val bb = Array(Array(1,1,3),Array(1,2,3),Array(1,3,4))
         val aa = Array(0,1,2)
           for{ a <- aa
                b <- bb(a)
           } yield a+b+await(T1.cbi(0))
           //val s = aa.flatMap(a => bb(a).map(b => a+b+await(T1.cbi(0))))
           //aa.flatMap(a => bb(await(T1.cbi(a))))
      }
      val r = c.run().get
      assert(r(0)==1)
      assert(r(1)==1)
      assert(r(2)==3)
      assert(r(3)==2)

  @Test def testFold1(): Unit =
     val c = async[ComputationBound]{
          Array(1,2,3).fold(0)(
                   (x:Int,y:Int) => x + y + await(T1.cbi(1)) 
          )
     }
     val l = c.run()
     assert(l == Success(9)) 

  @Test def testCollectFirst(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c = Array(1,2,3,4)
          val r: Option[Int] = c.collectFirst{ case x if x > 2 => x + await(T1.cbi(1)) }
          //val r: Option[Int] = await(T1.cbt(Some(4)))
          r
     }
     assert(c.run() == Success(Some(4)))

  @Test def testCollect(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c = Array(1,2,3,4)
          c.collect{ case x if x % 2 == 0 => x + await(T1.cbi(1)) }
     }
     val r = c.run().get
     assert(r(0)==3)
     assert(r(1)==5)


  @Test def testGroupByEmpty(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:Array[String] = Array()
          c.groupBy(x => await(T1.cbi(x.length)))
     }
     assert(c.run() == Success(Map.empty[Int,String]))

  @Test def testGroupBy(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c:Array[String] = Array("","a","aa","bb","aaa","bbb","ccc")
          c.groupBy(x => await(T1.cbi(x.length)))
     }
     val r = c.run().get
     assert(r(0).toList.size == 1)
     assert(r(1).toList.size == 1)
     assert(r(2).toList.size == 2)
     assert(r(3).toList.size == 3)


  @Test def testFoldLeft(): Unit =
     val c = async[ComputationBound]{
          Array(1,2,3).foldLeft("")((s,e) => "(" + s + "," + await(T1.cbi(e)).toString + ")" )
     }
     assert(c.run() == Success("(((,1),2),3)"))

  @Test def testFoldRight(): Unit =
     val c = async[ComputationBound]{
          Array(1,2,3).foldRight("")((e,s) => "(" + await(T1.cbs(e.toString)) + "," + s + ")" )
     }
     assert(c.run() == Success("(1,(2,(3,)))"))


  @Test def forall1(): Unit =
     val c = async[ComputationBound]{
          Array(1,2,3).forall(_ == await(T1.cbi(3)))
     }
     assert(c.run() == Success(false))

  @Test def forall2(): Unit =
     val c = async[ComputationBound]{
          Array(1,2,3).forall(_ > await(T1.cbi(0)))
     }
     assert(c.run() == Success(true))

  @Test def forall3(): Unit =
     val c = async[ComputationBound]{
          Array[Int]().forall(_ == await(T1.cbi(0)))
     }
     assert(c.run() == Success(true))

  @Test def testGroupMap1(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
          val c: Array[String] = Array("","a","aa","bb","aaa","bbb","ccc")
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
          val c = Array("","a","aa","bb","aaa","bbb","ccc")
          c.groupMap(x => await(T1.cbi(x.length)))(x => await(T1.cbs(x+"1")))
     }
     val r = c.run().get
     assert(r(0).toList.size == 1)
     assert(r(0)(0) == "1")
     assert(r(1).toList.size == 1)
     assert(r(1)(0) == "a1")
     assert(r(2).toList.size == 2)
     assert(r(3).toList.size == 3)

  /*
  bug in dotty: https://github.com/lampepfl/dotty/issues/17445
  @Test def testIndexWhere(): Unit =
     val c = async[ComputationBound]{
          val c = Array("","a","aa","bb","aaa","bbb","ccc")
          c.indexWhere(_.length == await(T1.cbi(3)))
     }
     val r = c.run().get
     assert(r == 4)
   */

  /*
  bug in dotty: https://github.com/lampepfl/dotty/issues/17445
  @Test def testLastIndexWhere(): Unit =
     val c = async[ComputationBound]{
          val c = Array("","a","aa","bb","aaa","bbb","ccc")
          c.lastIndexWhere(_.length == await(T1.cbi(3)))
     }
     val r = c.run().get
     assert(r == 6)

  @Test def testLastIndexWhereZero(): Unit =
     val c = async[ComputationBound]{
          val c = Array("","a","aa","bb","aaa","bbb","ccc")
          c.lastIndexWhere(_.length == await(T1.cbi(0)))
     }
     val r = c.run().get
     assert(r == 0)


  @Test def testLastIndexWhereEmpty(): Unit =
     val c = async[ComputationBound]{
          val c = Array[String]()
          c.lastIndexWhere(_.length == await(T1.cbi(3)))
     }
     val r = c.run().get
     assert(r == -1)
  */


  @Test def testPartition(): Unit =
     val c = async[ComputationBound]{
        val l = Array(1,2,3,4,5,6,7,8,3,1)
        l.partition( _ < await(T1.cbi(5)) )
     }
     val r = c.run().get
     assert(r._1.toVector == Vector(1,2,3,4,3,1))
     assert(r._2.toVector == Vector(5,6,7,8))


  @Test def testPartitionMap(): Unit =
      val c = async[ComputationBound]{
         val l = Array(1,2,3,4,5,6,7,8,3,1)
         l.partitionMap( x => if (x < await(T1.cbi(5))) Left(x) else Right(x.toString) )
      }
      val r = c.run().get
      assert(r._1.toVector == Vector(1,2,3,4,3,1))
      assert(r._2.toVector == Vector("5","6","7","8"))
 


  @Test def testScanLeft(): Unit =
     val c = async[ComputationBound]{
          val l = (1 to 5).toArray
          l.scanLeft(0)( (x,y) => x + await(T1.cbi(y)) )
     }
     val r = c.run().get
     assert(r.toSeq == Seq(0,1,3,6,10,15))

  
  @Test def testScanRight(): Unit =
     val c = async[ComputationBound]{
          val l = (1 to 5).toArray
          l.scanRight(0)( (x,y) => x + await(T1.cbi(y)) )
     }
     val checkC = {
       val l = (1 to 5).toArray
       l.scanRight(0)( (x,y) => x + y )  
     }
     val r = c.run().get
     assert(r.toSeq == checkC.toSeq)
     assert(r.toSeq == Seq(15,14,12,9,5,0))

  @Test def testSpan(): Unit =
      val c = async[ComputationBound]{
         val l = Array(1,2,3,4,5,6,7,8,3,1)
         l.span( _ < await(T1.cbi(5)) )
      }
      val r = c.run().get
      assert(r._1.toVector == Vector(1,2,3,4))
      assert(r._2.toVector == Vector(5,6,7,8,3,1))
 

  @Test def testTakeWhile(): Unit =
     val c = async[ComputationBound]{
        val l = Vector(1,2,3,4,5,6,7,8,3,1)
        l.takeWhile( _ < await(T1.cbi(5)) )
     }
     val r = c.run().get
     assert(r == Vector(1,2,3,4))

   
  @Test def testWithFilterSmoke(): Unit =
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        (0 to 9).toArray.withFilter(x => await(T1.cbi(0)) == x%2).map(_.toString) 
     }
     val r = c.run().get
     assert(r.toSeq == Seq("0","2","4","6","8") )
  



