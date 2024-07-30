package injection.examples.randomgen.simple

import canoe.api.*
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import injection.examples.randomgen.simple.telegram.{ChooseWinnerScenario, Token}

object Main extends IOApp {
  given token: Token = Token("<token>")
  override def run(args: List[String]): IO[ExitCode] = Stream
      .resource(TelegramClient[IO](token.underlying))
      .flatMap(implicit client => Bot.polling[IO].follow(ChooseWinnerScenario.flow))
      .compile
      .drain
      .map(_ => ExitCode.Success)
  }