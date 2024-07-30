package injection.examples.randomgen.simple.telegram

import canoe.api.*
import canoe.methods.files.GetFile
import canoe.models.messages.TextMessage
import canoe.models.{Chat, File}
import canoe.syntax.*
import cats.effect.{Async, Sync}
import injection.examples.randomgen.simple.generator.RandomOrgIntUniqueSequenceGenerator
import injection.examples.randomgen.simple.parser.PersonParser
import injection.examples.randomgen.simple.repository.StringRepository
import injection.examples.randomgen.simple.service.PersonFindWinners
import org.http4s.blaze.client.BlazeClientBuilder

object RandomGeneratorScenario {
  def flow[F[_] : TelegramClient : Async : Sync](using token: Token): Scenario[F, Unit] = {
    for {
      chat <- Scenario.expect(command("document").chat)
      text <- specifyDocument(chat)
      repository = new StringRepository[F](text)
      count <- Scenario.eval(repository.count)
      number <- specifyNumber(chat)(count)
      finder = new PersonFindWinners(repository, new RandomOrgIntUniqueSequenceGenerator, PersonParser)
      persons <- Scenario.eval(finder(number))
      _ <- Scenario.eval(chat.send(
        s"""Winners:
           |${persons.map(_.underlying).mkString("\n")}""".stripMargin))
    } yield ()
  }.stopOn(command("cancel").isDefinedAt)

  private def specifyDocument[F[_] : TelegramClient : Async](chat: Chat)(using token: Token): Scenario[F, String] = for {
    _ <- Scenario.eval(chat.send("Specify the .txt file from which to read"))
    documentMessage <- Scenario.expect(document)
    fileInfo <- Scenario.eval(GetFile(documentMessage.fileId).call[F, File])
    file <- fileInfo.filePath match
      case Some(filePath) =>
        Scenario.eval(telegramFile(filePath))
      case None =>
        specifyDocument(chat)
  } yield file


  private def specifyNumber[F[_] : TelegramClient : Sync](chat: Chat)(max: Long)(using token: Token): Scenario[F, Int] = for {
    _ <- Scenario.eval(chat.send(s"Specify the number of winners, less than or equal to $max"))
    text <- Scenario.expect(text)
    number <- text.toIntOption.filter(_ <= max) match
      case Some(value) => Scenario.eval(Sync[F].pure(value))
      case None => specifyNumber(chat)(max)
  } yield number


  private def telegramFile[F[_] : TelegramClient : Async](filePath: String)(using token: Token): F[String] =
    BlazeClientBuilder[F].resource.use { client =>
      client.expect[String](s"https://api.telegram.org/file/bot${token.underlying}/$filePath")
    }
}
