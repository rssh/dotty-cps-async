package cps

import scala.util._
import org.junit.{Test,Ignore}
import org.junit.Assert._

enum CountSignal[+T]:
 case Data(data: T)
 case Finish
  

object CBSWordCount1:

  def generate(line: String, channel:ASChannel[ComputationBound, CountSignal[String]]):ComputationBound[Unit] = {
     implicit val printCode = cps.macroFlags.PrintCode
     implicit val debugLevel = cps.macroFlags.DebugLevel(20)
     val r = async {
       val words = line.split(" ") 
       for(w <- words) {
         println(s"before write data $w")
         await(channel.write(CountSignal.Data(w)))
         println(s"written data $w")
       }
       await(channel.write(CountSignal.Finish))
       println(s"written finish")
     }
     r
  }

  def accept(channel:ASChannel[ComputationBound, CountSignal[String]]): ComputationBound[Map[String,Int]] = async {
     var state: Map[String,Int] = Map.empty
     var finished = false
     while {
       println("accept: before read")
       val w = await(channel.read())
       println(s"readed $w")
       w match 
         case CountSignal.Data(d) =>
               state = state.updated(d,
                     state.getOrElse(d,0)+1
               )
         case CountSignal.Finish =>
               finished = true
       !finished
     } do ()
     state
  }


class TestCBSWordCount:

  def qqq = ???

/*
  // can fail now
  @Test def tWordCount(): Unit = 
     val ch = new ASChannel[ComputationBound, CountSignal[String]]();
     val generator = ComputationBound.spawn(CBSWordCount1.generate("A A A",ch))
     val acceptor = CBSWordCount1.accept(ch)
     val c = acceptor.run()
     assert(c == Success(Map("A" -> 3)))
*/
  


