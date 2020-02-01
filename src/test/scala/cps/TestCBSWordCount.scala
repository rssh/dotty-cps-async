package cps

import scala.util._
import org.junit.{Test,Ignore}
import org.junit.Assert._

enum CountSignal[+T]
 case Data(data: T)
 case Finish
  

object CBSWordCount 
  
/*
  def generate(line: String, channel:ASChannel[ComputationBound, CountSignal[String]]):ComputationBound[Unit] = async {
     val words = line.split(" ") 
     for(w <- words) {
        await(channel.write(CountSignal.Data(w)))
     }
     await(channel.write(CountSignal.Finish))
  }

  def accept(channel:ASChannel[ComputationBound, CountSignal[String]]): ComputationBound[Map[String,Int]] = async {
     var state: Map[String,Int] = Map.empty
     var finished = false
     while {
       val w = await(channel.read())
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

*/

class TestCBSWordCount

  //@Ignore
  //@Test def tWordCount(): Unit = 
  //   val ch = new ASChannel[ComputationBound, CountSignal[String]]();
  //   val generator = ComputationBound.spawn(CBSWordCount.generate("A A A",ch))
  //   val acceptor = CBSWordCount.accept(ch)
  //   val c = acceptor.run()
  //   assert(c == Success(Map("A" -> 3)))
  


