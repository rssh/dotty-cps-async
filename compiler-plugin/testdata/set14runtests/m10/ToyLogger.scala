package cps.pe

import scala.annotation.experimental

import cps.*
import cps.testconfig.given

class ToyLogger{

    var lines: Vector[String] = Vector()

    def log(message:String): Unit =
      println(s"run log ${message} this=${this}")
      lines = lines :+ message

}

@experimental
class PEToyLogger{

   private val logger = new ToyLogger()

   def log(msg:String)(using CpsDirect[PureEffect]): Unit =
      logger.log(msg)

   def all()(using CpsDirect[PureEffect]): Vector[String] =
      logger.lines

   def __all(): Vector[String] =
      logger.lines

}

object PEToyLogger {

   def make()(using CpsDirect[PureEffect]): PEToyLogger =
     new PEToyLogger

}


