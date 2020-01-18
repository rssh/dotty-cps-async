package cps

import org.junit.{Test,Ignore}
import org.junit.Assert._




class TestBS1While

  def cbBool(b:Boolean): ComputationBound[Boolean] = ???

 // Dotty crash.
 // TODO: minimize and submit bug.
 //
  @Test def tWhileC1_11(): Unit = 
     val c = Async.transform[ComputationBound,Unit]{
        while(await(cbBool(false))) {
          await(cbBool(false))
        }
     }
     assert(true)



