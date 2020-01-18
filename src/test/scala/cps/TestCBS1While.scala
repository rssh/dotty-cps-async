package c

import org.junit.{Test,Ignore}
import org.junit.Assert._

class TestBS1While

  def cbBool(b:Boolean): CB[Boolean] = ???

 // Dotty crash.
 // TODO: minimize and submit bug.
 //
  @Test def tWhileC1_11(): Unit = 
     val c = Async.transform[Unit]{
        while(await(cbBool(false))) {
          await(cbBool(false))
        }
     }
     assert(true)



