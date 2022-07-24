package cps.gopherlike

import org.junit.{Test,Ignore}
import org.junit.Assert._

import scala.quoted._
import scala.util.Success

import cps._
import cps.testconfig.given

class TestIF:

  @Test def reproduce(): Unit =
     //implicit val printCode = cps.macros.flags.PrintCode
     //implicit val printTree = cps.macroFlags.PrintTree
     //implicit val debugLevel = cps.macros.flags.DebugLevel(20)
     val writer = CIFWriter[ComputationBound,Int]()
     val reader = CIFReader[ComputationBound,Int](10)
     val c = async{
        // compiler crash:
        //     https://github.com/lampepfl/dotty/issues/10910
        reader.foreach{
           a => writer.write(a)
        }
        //await(reader.aforeach{
        //   a => writer.write(a)
        //})
        //reader.aforeach{
        //   a => writer.write(a)
        //}
        // happy path
        //reader.aforeach{
        //     a => await(writer.awrite(a))
        //}
        writer.v
     }
     assert(c.run() == Success(10))

