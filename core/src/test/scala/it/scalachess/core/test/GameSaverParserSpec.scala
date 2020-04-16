package it.scalachess.core.test

import it.scalachess.core.board.Position
import it.scalachess.core.logic.moves.{ AlgebraicCastling, AlgebraicSimpleMove, KingSide, QueenSide }
import it.scalachess.core.parser.GameSaverParser
import it.scalachess.core.pieces.{ Pawn, Rook }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class GameSaverParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  val saver: GameSaverParser = GameSaverParser()

  "The GameSaver parser " should "be able to parse algebraic move into pgn format with ambiguity" in {
    saver
      .parse(
        AlgebraicSimpleMove(Position(1, 3), Pawn, capture = false, check = false, checkmate = true, None, None, None))
      .toOption
      .value shouldEqual "a3#"
  }

  it should "be able to parse a capture" in {
    saver
      .parse(
        AlgebraicSimpleMove(Position(1, 3),
                            Pawn,
                            capture = true,
                            check = false,
                            checkmate = true,
                            Some('a'),
                            None,
                            None))
      .toOption
      .value shouldEqual "axa3#"
  }

  it should "be able to parse a king side castling" in {
    saver
      .parse(AlgebraicCastling(KingSide, check = false, checkmate = false))
      .toOption
      .value shouldEqual "0-0"
  }

  it should "be able to parse a queen side castling" in {
    saver
      .parse(AlgebraicCastling(QueenSide, check = false, checkmate = true))
      .toOption
      .value shouldEqual "0-0-0#"
  }

  it should "be able to parse a promotion" in {
    saver
      .parse(
        AlgebraicSimpleMove(Position(1, 8),
                            Pawn,
                            capture = false,
                            check = false,
                            checkmate = true,
                            None,
                            None,
                            Some(Rook)))
      .toOption
      .value shouldEqual "a8=R#"
  }
}
