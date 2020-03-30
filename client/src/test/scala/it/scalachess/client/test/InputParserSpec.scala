package it.scalachess.client.test

import it.scalachess.client.remoteClient.Client.{ Create, Forfeit, Join, ParsedMove }
import it.scalachess.client.remoteClient.InputParser
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class InputParserSpec extends FlatSpec with Matchers with OptionValues with Inspectors {
  private val create: String  = "/create"
  private val forfeit: String = "/forfeit"
  private val joinCommands: Map[String, String] =
    Map("/join 12423423" -> "12423423", "/join 1" -> "1", "/join 1242" -> "1242")
  private val invalidJoinCommands: Seq[String]   = Seq("join", "/join", "/join ", create, forfeit)
  private val invalidCreateCommands: Seq[String] = Seq("/create 1234", "create", "/creat", "/join 1234", forfeit)
  private val invalidForfeitCommands             = Seq("/forfeit 1234", "forfeit", "/forfei", "/join 1234", create)
  private val moves: Seq[String]                 = Seq("a3", "axa5", "bx")

  "Join commands" should "be parsed correctly" in {
    forAll(joinCommands) { case (input, id) => InputParser.inputToCommand(input) should be(Join(id)) }
    forAll(invalidJoinCommands) { InputParser.inputToCommand(_) should not be a[Join] }
  }

  "Create command" should "be parsed" in {
    InputParser.inputToCommand(create) should be(Create)
    forAll(invalidCreateCommands) { InputParser.inputToCommand(_) should not be a[Create.type] }
  }

  "Forfeit command" should "be parsed" in {
    InputParser.inputToCommand(forfeit) should be(Forfeit)
    forAll(invalidForfeitCommands) { InputParser.inputToCommand(_) should not be a[Forfeit.type] }
  }

  "All other commands" should "be parsed as Move" in {
    forAll(moves) { m =>
      InputParser.inputToCommand(m) should be(ParsedMove(m))
    }
    forAll(Seq(create, forfeit) ++ joinCommands.keys) { InputParser.inputToCommand(_) should not be a[ParsedMove] }
  }
}
