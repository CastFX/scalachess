package it.scalachess.client.remote_client.test

import akka.actor.testkit.typed.scaladsl.{ BehaviorTestKit, TestInbox }
import it.scalachess.client.remote_client
import it.scalachess.client.remote_client.ClientCommands.{ ClientCommand, Create, Forfeit, Help, InputMove, Join }
import it.scalachess.client.remote_client.InputParser
import it.scalachess.util.NetworkMessages.ClientMessage
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class InputParserSpec extends FlatSpec with Matchers with OptionValues with Inspectors {
  private val create: String              = "/create"
  private val forfeit: String             = "/forfeit"
  private val help: String                = "/help"
  private val joinCommands                = Map("/join 12423423" -> "12423423", "/join 1" -> "1", "/join 1242" -> "1242")
  private val moves: Seq[String]          = Seq("a3", "axa5", "bx")
  private lazy val invalidJoinCommands    = Seq("join", "/join", "/join ", create, forfeit, help) ++ moves
  private lazy val invalidCreateCommands  = Seq("/create 12", "create", "/creat", forfeit, help) ++ joinCommands.keys ++ moves
  private lazy val invalidForfeitCommands = Seq("/forfeit 12", "forfeit", "/forfei", "/join 1234", create, help) ++ joinCommands.keys ++ moves
  private lazy val invalidHelpCommands    = Seq("help", "help", "\\help", "/hel", create, forfeit) ++ joinCommands.keys ++ moves

  "Join commands" should "be parsed correctly" in {
    val clientInbox = TestInbox[ClientMessage]()
    val inputParser = BehaviorTestKit(InputParser(clientInbox.ref))
    forAll(joinCommands) {
      case (join, id) =>
        inputParser run join
        clientInbox expectMessage Join(id)
    }

    invalidJoinCommands foreach inputParser.run
    all(clientInbox.receiveAll()) should not be a[Join]
  }

  "Create, Forfeit and Help commands" should "be parsed correctly" in {
    testSingleWordCommand(create, invalidCreateCommands, Create)
    testSingleWordCommand(forfeit, invalidForfeitCommands, Forfeit)
    testSingleWordCommand(help, invalidHelpCommands, Help)
  }

  "All other commands" should "be parsed as Move" in {
    val clientInbox = TestInbox[ClientMessage]()
    val inputParser = BehaviorTestKit(remote_client.InputParser(clientInbox.ref))
    forAll(moves) { move =>
      inputParser run move
      clientInbox expectMessage InputMove(move)
    }

    (Seq(create, forfeit) ++ joinCommands.keys) foreach inputParser.run
    all(clientInbox.receiveAll()) should not be a[InputMove]
  }

  private def testSingleWordCommand(command: String, notCommands: Seq[String], result: ClientCommand): Unit = {
    val clientInbox = TestInbox[ClientMessage]()
    val inputParser = BehaviorTestKit(remote_client.InputParser(clientInbox.ref))
    inputParser run command
    clientInbox expectMessage result

    notCommands foreach inputParser.run
    all(clientInbox.receiveAll()) should not be a[result.type]
  }
}
