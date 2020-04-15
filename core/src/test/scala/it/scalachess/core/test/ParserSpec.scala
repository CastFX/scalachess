package it.scalachess.core.test

import it.scalachess.core.board.Position
import it.scalachess.core.logic.moves.{ AlgebraicCastling, AlgebraicSimpleMove, KingSide, QueenSide }
import it.scalachess.core.parser.Parser
import it.scalachess.core.parser.Parser.AlgebraicParser
import it.scalachess.core.pieces.{ Bishop, Pawn, Queen }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class ParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  val parser: Parser.AlgebraicParser.type = AlgebraicParser

  "The Algebraic parser" should "be able to parse moves in algebraic notations" in {
    val movesToBeParsed: Seq[String] =
      Seq("e4", "Re4", "0-0", "0-0-0", "Rae8", "Ra6+", "a4#", "Nb6d7", "exe4", "Qxe4")
    parser.parseAll(movesToBeParsed).foreach(move => move.toOption.isDefined shouldBe true)
  }

  it should "not be able to parse invalid moves" in {
    val invalidMovesToBeParsed: Seq[String] =
      Seq("move", "a", "aa", "33", "+", "#", "xe3", "", " ")
    parser.parseAll(invalidMovesToBeParsed).foreach(move => move.toOption.isEmpty shouldBe true)
  }

  it should "be able to parse a castling king side" in {
    parser.parse("0-0").toOption.value shouldEqual AlgebraicCastling(KingSide, check = false, checkmate = false)
    parser.parse("0-0+").toOption.value shouldEqual AlgebraicCastling(KingSide, check = true, checkmate = false)
    parser.parse("0-0#").toOption.value shouldEqual AlgebraicCastling(KingSide, check = false, checkmate = true)
  }

  it should "be able to parse a castling queen side" in {
    parser.parse("0-0-0").toOption.value shouldEqual AlgebraicCastling(QueenSide, check = false, checkmate = false)
    parser.parse("0-0-0+").toOption.value shouldEqual AlgebraicCastling(QueenSide, check = true, checkmate = false)
    parser.parse("0-0-0#").toOption.value shouldEqual AlgebraicCastling(QueenSide, check = false, checkmate = true)
  }

  it should "be able to parse a move with capture" in {
    val capture  = true
    val position = Position.ofNotation("a2").value
    parser.parse("Bxa2").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Bishop, capture, check = false, checkmate = false, None, None, None)
    parser.parse("exa2").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = false, checkmate = false, Some('e'), None, None)
    parser.parse("2xa2").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = false, checkmate = false, None, Some(2), None)
    parser.parse("e2xa2").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = false, checkmate = false, Some('e'), Some(2), None)
  }

  it should "be able to parse a move with check" in {
    val position = Position.ofNotation("a4").value
    val capture  = false
    parser.parse("a4+").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = true, checkmate = false, None, None, None)
  }

  it should "be able to parse a move with checkmate" in {
    val capture  = false
    val position = Position.ofNotation("a3").value
    parser.parse("a3#").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = false, checkmate = true, None, None, None)
  }

  it should "be able to parse a move with promotion" in {
    val capture  = false
    val position = Position.ofNotation("a3").value
    parser.parse("a3=Q").toOption.value shouldEqual
    AlgebraicSimpleMove(position, Pawn, capture, check = false, checkmate = false, None, None, Some(Queen))
  }

  it should "not be able to parse a move with capture with too many arguments" in {
    parser.parse("NeNxa7=Q").toOption.isEmpty shouldBe true
    parser.parse("Neexa7=Q").toOption.isEmpty shouldBe true
  }
}
