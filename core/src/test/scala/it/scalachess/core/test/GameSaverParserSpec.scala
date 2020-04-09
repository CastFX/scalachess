package it.scalachess.core.test

import it.scalachess.core.{ ChessGame, White, Win }
import it.scalachess.core.parser.{ GameSaverParser, Parser }
import it.scalachess.core.parser.Parser.AlgebraicParser
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class GameSaverParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  val parser: Parser.AlgebraicParser.type = AlgebraicParser

  "The GameSaver parser" should "be able to create a string of the game moves" in {
    var game: ChessGame       = ChessGame.standard()
    val firstMoveWhitePawn    = "e4"
    val secondMoveBlackPawn   = "e5"
    val thirdMoveWhiteBishop  = "Bc4"
    val fourthMoveBlackKnight = "Nc6"
    val fifthMoveWhiteQueen   = "Qh5"
    val sixthMoveBlackKnight  = "Nf6"
    val seventhMoveWhiteQueen = "Qxf7#"
    val result: String = s"1.e2e4 e7e5\n" +
    s"2.Bf1c4 Nb8c6\n" +
    s"3.Qd1h5 Ng8f6\n" +
    s"4.Qh5xf7#\n"
    game = game(firstMoveWhitePawn).toOption.value
    game = game(secondMoveBlackPawn).toOption.value
    game = game(thirdMoveWhiteBishop).toOption.value
    game = game(fourthMoveBlackKnight).toOption.value
    game = game(fifthMoveWhiteQueen).toOption.value
    game = game(sixthMoveBlackKnight).toOption.value
    game = game(seventhMoveWhiteQueen).toOption.value
    game.gameStatus should equal(Win(White))
    game.isKingInCheck shouldBe true
    GameSaverParser.parseAndConvert(game.moveHistory) shouldEqual result
  }

}
