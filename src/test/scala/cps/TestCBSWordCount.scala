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
         println(s"writer:before write $w")
         await(channel.write(CountSignal.Data(w)))
         println(s"writee:after write $w")
       }
       println("writer:before write finish")
       await(channel.write(CountSignal.Finish))
       println("writer:after write finish")
     }
     r
  }

  def accept(channel:ASChannel[ComputationBound, CountSignal[String]]): ComputationBound[Map[String,Int]] = async {
     var state: Map[String,Int] = Map.empty
     var finished = false
     while {
       println(s"reader:before read")
       val w = await(channel.read())
       println(s"reader: read $w")
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

  @Test def tWordCount(): Unit = 
     val ch = new ASChannel[ComputationBound, CountSignal[String]]();
     val generator = ComputationBound.spawn(CBSWordCount1.generate("A A A",ch))
     val acceptor = CBSWordCount1.accept(ch)
     val c = acceptor.run()
     assert(c == Success(Map("A" -> 3)))
  


