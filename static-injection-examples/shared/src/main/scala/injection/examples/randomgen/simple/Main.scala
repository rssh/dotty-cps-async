package injection.examples.randomgen.simple

import canoe.api.*
import cats.effect.{ExitCode, IO, IOApp}
import cps.monads.catsEffect.{*, given}
import fs2.Stream
import injection.examples.randomgen.simple.telegram.{ChooseWinnerScenario, Token}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    given token: Token = Token(args.head)

    Stream
      .resource(TelegramClient[IO](token.underlying))
      .flatMap(implicit client => Bot.polling[IO].follow(ChooseWinnerScenario.flow))
      .compile
      .drain
      .map(_ => ExitCode.Success)
}