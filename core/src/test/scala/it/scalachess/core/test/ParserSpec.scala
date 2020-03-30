package it.scalachess.core.test

import it.scalachess.core.board.Position
import it.scalachess.core.logic.moves.{ Castling, KingSide, ParsedSimpleMove, QueenSide }
import it.scalachess.core.parser.Parser.AlgebraicParser
import it.scalachess.core.pieces.{ Bishop, Pawn, Queen }
import org.scalatest.{ FlatSpec, Inspectors, Matchers }

class ParserSpec extends FlatSpec with Matchers with Inspectors {

  val parser: AlgebraicParser = AlgebraicParser()

  "The parser" should "be able to parse moves in algebraic notations" in {
    val movesToBeParsed: Seq[String] =
      Seq("e4", "Re4", "0-0", "0-0-0", "Rae8", "Ra6+", "a4#", "Nb6d7", "exe4", "Qxe4")
    parser.parseAll(movesToBeParsed).foreach(move => move.isDefined shouldBe true)
  }

  it should "not be able to parse invalid moves" in {
    val invalidMovesToBeParsed: Seq[String] =
      Seq("move", "a", "aa", "33", "+", "#", "xe3", "", " ")
    parser.parseAll(invalidMovesToBeParsed).foreach(move => move.isDefined shouldBe false)
  }

  it should "be able to parse a castling king side" in {
    parser.parse("0-0") shouldEqual Some(Castling(KingSide, check = false, checkmate = false))
    parser.parse("0-0+") shouldEqual Some(Castling(KingSide, check = true, checkmate = false))
    parser.parse("0-0#") shouldEqual Some(Castling(KingSide, check = false, checkmate = true))
  }

  it should "be able to parse a castling queen side" in {
    parser.parse("0-0-0") shouldEqual Some(Castling(QueenSide, check = false, checkmate = false))
    parser.parse("0-0-0+") shouldEqual Some(Castling(QueenSide, check = true, checkmate = false))
    parser.parse("0-0-0#") shouldEqual Some(Castling(QueenSide, check = false, checkmate = true))
  }

  it should "be able to parse a move with capture" in {
    val position = Position.ofNotation("a2").get
    parser.parse("Bxa2") shouldEqual Some(
      ParsedSimpleMove(position, Bishop, (Some(Bishop), None), check = false, checkmate = false, None, None, None))
    parser.parse("exa2") shouldEqual Some(
      ParsedSimpleMove(position, Pawn, (Some(Pawn), Some('e')), check = false, checkmate = false, None, None, None))
  }

  it should "be able to parse a move with check" in {
    val position = Position.ofNotation("a4").get
    parser.parse("a4+") shouldEqual Some(
      ParsedSimpleMove(position, Pawn, (None, None), check = true, checkmate = false, None, None, None))
  }

  it should "be able to parse a move with checkmate" in {
    val position = Position.ofNotation("a3").get
    parser.parse("a3#") shouldEqual Some(
      ParsedSimpleMove(position, Pawn, (None, None), check = false, checkmate = true, None, None, None))
  }

  it should "be able to parse a move with promotion" in {
    val position = Position.ofNotation("a3").get
    parser.parse("a3=Q") shouldEqual Some(
      ParsedSimpleMove(position, Pawn, (None, None), check = false, checkmate = false, None, None, Some(Queen)))
  }

  it should "not be able to parse a move with promotion made by not a pawn" in {
    parser.parse("Ra5=Q").isDefined shouldBe false
  }
}
