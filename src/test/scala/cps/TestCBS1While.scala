package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success



class TestBS1While


 // Dotty crash.
 // TODO: minimize and submit bug.
 //
  @Test def tWhileC1_11(): Unit = 
     val c = Async.transform[ComputationBound,Unit]{
        while(await(T1.cbBool(false))) {
          await(T1.cbi(1))
        }
     }
     assert(true)



