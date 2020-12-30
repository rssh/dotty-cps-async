package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success
import scala.util.Failure


class TestBS1ShiftSeq:


  @Test def testDistinctBy(): Unit = 
     //implicit val printCode = cps.macroFlags.PrintCode
     //implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val c = async[ComputationBound]{
        val seq = Seq("1234","3452","1","12","21","777777777")
        seq.distinctBy{ x => T1.cbi(x.length) }
     }
     c.run() match 
        case Success(s@Seq(a,b,c,d)) => assert( s.filter(_.length == 1).length == 1 )
                               assert( s.filter(_.length == 2).length == 1 )
                               assert( s.filter(_.length == 4).length == 1 )
        case _ => assert( "" == "shoud be seq from 4 elements") 




