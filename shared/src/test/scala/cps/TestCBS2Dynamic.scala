package cps

import org.junit.{Test,Ignore}
import org.junit.Assert.*

import scala.util.*

import cps.automaticColoring
import scala.language.implicitConversions
import scala.language.dynamics


class TestCBS2Dynamic:

  class MyDynamic(log:Log, mockMap: Map[String,Any]) extends Dynamic:

    def applyDynamic(methodName: String)(args: Any*):Any =
      log.log(s"MyDynamic:applyDynamic, methodName: $methodName, args: $args")
      mockMap.get(methodName) match
        case Some(r) => r
        case other => "?"
        
  given myDynamicAsync: AsyncShift[MyDynamic] with

     def applyDynamic[F[_]](o:MyDynamic, m:CpsMonad[F])(methodName: String)(args: Any*):Any =
       o.runAsync(methodName, m, args)

  
  class Log:
    private var lines = Vector[String]()

    def log(msg:String): Unit =
       lines = lines :+ msg

    def all: Vector[String] = lines



  @Test def testApplyDynamic(): Unit = 
     val log = new Log()
     val myDynamic = new MyDynamic(log,Map("f"->1, "runAsync"->3))
     //implicit val printCode = cps.macroFlags.PrintCode
     val c = async {
        val r1 = myDynamic.f(1,(x:Int) => x+1)
        val r2 = myDynamic.f(2,(x:Int) => x+await(T1.cbi(1)))
        r2
     }
     val r: Try[Any] = c.run()
     assert(r.isSuccess)
     r match
       case Success(v) => assert(v == 3)
       case Failure(ex) => throw ex
     val lines = log.all
     assert(lines(0).startsWith("MyDynamic:applyDynamic, methodName: f"))
     assert(lines(1).startsWith("MyDynamic:applyDynamic, methodName: runAsync"))



