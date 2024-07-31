package injection.examples.randomgen.plain.telegram

import canoe.api.*
import canoe.methods.files.GetFile
import canoe.models.messages.TextMessage
import canoe.models.{Chat, File}
import canoe.syntax.*
import cats.effect.{Async, Sync}
import cps.injection.plain.{*, given}
import injection.examples.randomgen.plain.generator.{*, given}
import injection.examples.randomgen.plain.parser.{*, given}
import injection.examples.randomgen.plain.repository.{*, given}
import injection.examples.randomgen.plain.service.{*, given}
import injection.examples.randomgen.plain.model.Person
import cps.{*, given}
import org.http4s.blaze.client.BlazeClientBuilder


object ChooseWinnerScenario {
  def flow[F[_] : TelegramClient : Async : CpsTryMonad](using token: Token, finder: FindWinners[F, Person]): Scenario[F, Unit] = async[[X] =>> Scenario[F, X]] {
    injectfull {
      given chat: Chat = await(Scenario.expect(command("document").chat))
      given repository: StringRepository[F] = new StringRepository[F](await(specifyDocument))
      val number = await(specifyNumber(await(Scenario.eval(repository.count))))
      val persons = await(Scenario.eval(inject[FindWinners[F, Person]](number)))
      await(Scenario.eval(chat.send(
        s"""Winners:
           |${persons.map(_.underlying).mkString("\n")}""".stripMargin)))
      ()
    }
  }.stopOn(command("cancel").isDefinedAt)

  private def specifyDocument[F[_] : TelegramClient : Async](using token: Token, chat: Chat): Scenario[F, String] = async[[X] =>> Scenario[F, X]] {
    await(Scenario.eval(chat.send("Specify the text file from which to read")))
    val documentValue = await(Scenario.expect(document))
    val fileInfo = await(Scenario.eval(GetFile(documentValue.fileId).call[F, File]))
    fileInfo.filePath match
      case Some(filePath) =>
        await(Scenario.eval(telegramFile(filePath)))
      case None =>
        await(specifyDocument)
  }


  private def specifyNumber[F[_] : TelegramClient : Async](max: Long)(using token: Token, chat: Chat): Scenario[F, Int] = async[[X] =>> Scenario[F, X]] {
    await(Scenario.eval(chat.send(s"Specify the number of winners, less than or equal to $max")))
    val textValue = await(Scenario.expect(text))
    textValue.toIntOption.filter(_ <= max) match
      case Some(value) => await(Scenario.eval(Sync[F].pure(value)))
      case None => await(specifyNumber(max))
  }


  private def telegramFile[F[_] : TelegramClient : Async](filePath: String)(using token: Token): F[String] =
    BlazeClientBuilder[F].resource.use { client =>
      client.expect[String](s"https://api.telegram.org/file/bot${token.underlying}/$filePath")
    }
}
