package it.scalachess.core.test

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{
  FullMove,
  KingSide,
  QueenSide,
  ValidCastling,
  ValidEnPassant,
  ValidPromotion,
  ValidSimpleMove
}
import it.scalachess.core.{ Black, White, Win }
import it.scalachess.core.parser.{ GameSaverParser, Parser }
import it.scalachess.core.parser.Parser.AlgebraicParser
import it.scalachess.core.pieces.{ Pawn, Piece, Rook }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class GameSaverParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  val parser: Parser.AlgebraicParser.type = AlgebraicParser

  "The GameSaver parser " should "be able to parse full move into pgn format" in {
    val simple    = ValidSimpleMove(Position(1, 2), Position(1, 3), Pawn, White, None)
    val capture   = ValidSimpleMove(Position(1, 2), Position(1, 3), Pawn, White, Some(Position(1, 3)))
    val enPassant = ValidEnPassant(Position(1, 2), Position(1, 3), White, Position(1, 3))
    val kingSide  = ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide)
    val queenSide = ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide)
    val promotion = ValidPromotion(Position(1, 7), Position(1, 8), White, Piece(White, Rook), None)
    GameSaverParser
      .parse(FullMove(simple, resultsInCheck = true, resultsInCheckmate = true, Board.defaultBoard()))
      .toOption
      .value shouldEqual "a2a3#"
    GameSaverParser
      .parse(FullMove(capture, resultsInCheck = true, resultsInCheckmate = true, Board.defaultBoard()))
      .toOption
      .value shouldEqual "a2xa3#"
    GameSaverParser
      .parse(FullMove(enPassant, resultsInCheck = true, resultsInCheckmate = false, Board.defaultBoard()))
      .toOption
      .value shouldEqual "a2xa3+"
    GameSaverParser
      .parse(FullMove(kingSide, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard()))
      .toOption
      .value shouldEqual "0-0"
    GameSaverParser
      .parse(FullMove(queenSide, resultsInCheck = false, resultsInCheckmate = true, Board.defaultBoard()))
      .toOption
      .value shouldEqual "0-0-0#"
    GameSaverParser
      .parse(FullMove(promotion, resultsInCheck = false, resultsInCheckmate = true, Board.defaultBoard()))
      .toOption
      .value shouldEqual "a7a8=R#"
  }

  it should "be able to create a string of the game moves" in {
    val game = GameCreator.scholarGame
    val result: String = s"1.e2e4 e7e5\n" +
    s"2.Bf1c4 Nb8c6\n" +
    s"3.Qd1h5 Ng8f6\n" +
    s"4.Qh5xf7#\n" +
    s"1-0"
    game.gameStatus should equal(Win(White))
    game.isKingInCheck shouldBe true
    GameSaverParser.parseAndConvert(game.moveHistory, Some(Win(White))) shouldEqual result
  }
}
