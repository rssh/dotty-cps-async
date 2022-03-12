package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success



class TestCBS1Repeated:

  def qqq:Int = 0
  
  @Test def repeated_0(): Unit = 
     val c = async[ComputationBound]{
       val (b,c) = (2,4)
       s"a ${b} ${c}"
     }
     assert(c.run() == Success("a 2 4"))

  @Test def repeated_0c(): Unit = 
     val c = async[ComputationBound]{
       f("x",1,2,3,4)
     }
     assert(c.run() == Success("x1234"))

  @Test def repeated_0a(): Unit = 
     val c = async[ComputationBound]{
       val s = Seq(12,13,14)
       f("x",s :_* )
     }
     assert(c.run() == Success("x121314"))

  def f(x:String, y: Int*):String =
      x + y.mkString

  def f2(x:FC*):String =
      x.map(e => e.name+":"+e.fun()).mkString(",")

  case class FC(name:String, fun: ()=>Boolean)

  @Test def repeated_fun(): Unit = 
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val c = async[ComputationBound]{
       val fc1 = FC(
        "aaa", 
        () => ({val tmp=1; tmp})==2
       )
       val fc2 = FC("bbb", ()=>true) 
       val esx = Seq(fc1,fc2)
       f2(esx :_* )
     }
     assert(c.run() == Success("aaa:false,bbb:true"))


