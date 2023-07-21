package cps.utestlike


import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.plugin.annotation.*
import cps.macros.flags.UseCompilerPlugin
given UseCompilerPlugin.type = UseCompilerPlugin

@CpsDebugLevel(20)
class TestUtestLike:
  
  def qqq = 0

  
  @Test def reproduce(): Unit =
     val f = T1.cbi(10)
     val c = async{
        val r1 = await(f)
        UtestLikeMacro.assert( r1 == 2 )
     }
     assert(c.run() == Success(()))
  

// workarround for https://github.com/lampepfl/dotty/issues/11331
object TestUtestLike:

   def dummy=0


