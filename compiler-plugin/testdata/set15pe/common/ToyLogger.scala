package cps.pe

import cps.*
import scala.annotation.experimental


class ToyLogger{

    var lines: Vector[String] = Vector()

    def log(message:String): Unit =
      this.synchronized{
        lines = lines :+ message
      }


}

@experimental
class PEToyLogger {

   private val logger = new ToyLogger()

   def log(msg:String)(using CpsDirect[PureEffect]): Unit =
      await(PureEffect.delay(logger.log(msg)))

   def asyncLog(msg:String)(using CpsMonadContext[PureEffect]): PureEffect[Unit] =
      PureEffect.delay(logger.log(msg))

   def all()(using CpsDirect[PureEffect]): Vector[String] =
      await(PureEffect.delay(logger.lines))

   def __all(): Vector[String] =
      logger.lines
}

@experimental
object PEToyLogger {

   def make()(using CpsDirect[PureEffect]): PEToyLogger =
     await(PureEffect.delay(new PEToyLogger))

}


