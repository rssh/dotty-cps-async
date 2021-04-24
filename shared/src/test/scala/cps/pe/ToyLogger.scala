package cps.pe


class ToyLogger{

    var lines: Vector[String] = Vector()

    def log(message:String): Unit =
      lines = lines :+ message

}


class PEToyLogger{

   private val logger = new ToyLogger()

   def log(msg:String): PureEffect[Unit] =
      PureEffect.delay(logger.log(msg)) 

   def all(): PureEffect[Vector[String]] =
      PureEffect.delay(logger.lines) 

   def __all(): Vector[String] =
      logger.lines
}

object PEToyLogger {

   def make(): PureEffect[PEToyLogger] =
     PureEffect.delay(new PEToyLogger)

}


