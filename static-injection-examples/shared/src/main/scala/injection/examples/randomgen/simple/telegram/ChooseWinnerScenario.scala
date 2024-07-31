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
import cps.{*, given}
import org.http4s.blaze.client.BlazeClientBuilder


object ChooseWinnerScenario {
  def flow[F[_] : TelegramClient : Async : CpsTryMonad](using token: Token): Scenario[F, Unit] = async[[X] =>> Scenario[F, X]] {
    implicit val chat: Chat = await(Scenario.expect(command("document").chat))
    val text = await(specifyDocument)
    val repository = new StringRepository[F](text)
    val count = await(Scenario.eval(repository.count))
    val number = await(specifyNumber(count))
    val finder = new PersonFindWinners(repository, new RandomOrgIntUniqueSequenceGenerator, PersonParser)
    val persons = await(Scenario.eval(finder(number)))
    await(Scenario.eval(chat.send(
      s"""Winners:
         |${persons.map(_.underlying).mkString("\n")}""".stripMargin)))
    ()
  }.stopOn(command("cancel").isDefinedAt)

  private def specifyDocument[F[_] : TelegramClient : Async](using token: Token, chat: Chat): Scenario[F, String] = async[[X] =>> Scenario[F, X]] {
    await(Scenario.eval(chat.send("Specify the text file from which to read")))
    val documentMessage = await(Scenario.expect(document))
    val fileInfo = await(Scenario.eval(GetFile(documentMessage.fileId).call[F, File]))
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
