package it.scalachess.core.test

import it.scalachess.core.{ Black, ChessGame, Ongoing, White, Win }
import it.scalachess.core.board.Position
import it.scalachess.core.pieces.{ Pawn, Piece }
import org.scalatest.{ FlatSpec, Matchers, OptionValues }
import it.scalachess.core.test.ChessGameFailureMatcher.generateFailure

class ChessGameSpec extends FlatSpec with Matchers with OptionValues {
  var simpleGame: ChessGame              = ChessGame.standard()
  var testKingConstraintsGame: ChessGame = ChessGame.standard()
  var foolMateGame: ChessGame            = ChessGame.standard()
  var scholarMateGame: ChessGame         = ChessGame.standard()
  var simpleGameTurnCounter              = 0

  "A standard ChessGame" should "have a non-empty board" in {
    assert(simpleGame.board.pieces.nonEmpty)
  }

  it should "have a starting white player at turn 0" in {
    simpleGame.player should equal(White)
    simpleGame.turn should be(simpleGameTurnCounter)
  }

  it should "not accept a move with an illegal format" in {
    val illegalFormatMove = "aa2 a3"
    simpleGame(illegalFormatMove) should generateFailure
  }

  it should "not allows player to move an opponent piece" in {
    val blackPawnMove = "a7 a6"
    simpleGame(blackPawnMove) should generateFailure
  }

  it should "not allows a player captures his own pieces" in {
    val queenCapturesPawn = "Qxd2"
    simpleGame(queenCapturesPawn) should generateFailure
  }

  it should "not allows specific pieces pass through other pieces" in {
    val queenPassThroughPawn = "Qd3"
    simpleGame(queenPassThroughPawn) should generateFailure
  }

  it should "apply a move correctly" in {
    val whitePawnMove          = "a3"
    val whitePawnFinalPosition = Position(1, 3)
    simpleGame(whitePawnMove).toOption.value.board.pieceAtPosition(whitePawnFinalPosition).value should equal(
      Piece(White, Pawn))
  }

  it should "must alternate turns between players" in {
    val whitePawnMove = "a3"
    val blackPawnMove = "a6"
    simpleGame = simpleGame(whitePawnMove).toOption.value
    simpleGameTurnCounter += 1
    simpleGame.turn should be(simpleGameTurnCounter)
    simpleGame.player should equal(Black)
    simpleGame = simpleGame(blackPawnMove).toOption.value
    simpleGameTurnCounter += 1
    simpleGame.turn should be(simpleGameTurnCounter)
    simpleGame.player should equal(White)
  }

  "Build a game where the king " should "not being able do some move because it's under check" in {
    val whitePawnMove               = "e4"
    val firstBlackPawnMove          = "e5"
    val whiteBishopMove             = "Bd3"
    val secondBlackPawnMove         = "d5"
    val secondWhitePawnMove         = "f4"
    val blackQueenMove              = "Qh4+"
    val whiteKingMoveNotAllowed     = "Kf2"
    val whiteKingMoveAllowed        = "Ke2"
    val anotherWhiteKingMoveAllowed = "Kf1"
    testKingConstraintsGame = testKingConstraintsGame(whitePawnMove).toOption.value
    testKingConstraintsGame = testKingConstraintsGame(firstBlackPawnMove).toOption.value
    testKingConstraintsGame = testKingConstraintsGame(whiteBishopMove).toOption.value
    testKingConstraintsGame = testKingConstraintsGame(secondBlackPawnMove).toOption.value
    testKingConstraintsGame = testKingConstraintsGame(secondWhitePawnMove).toOption.value
    testKingConstraintsGame = testKingConstraintsGame(blackQueenMove).toOption.value
    testKingConstraintsGame.isKingInCheck shouldBe true
    testKingConstraintsGame.gameStatus should be(Ongoing)
    testKingConstraintsGame(whiteKingMoveNotAllowed) should generateFailure
    testKingConstraintsGame(whiteKingMoveAllowed).isSuccess should be(true)
    testKingConstraintsGame = testKingConstraintsGame(anotherWhiteKingMoveAllowed).toOption.value
    testKingConstraintsGame.isKingInCheck shouldBe false
  }

  /*
   * FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val firstWhitePawnMove  = "f3"
    val blackPawnMove       = "e6"
    val secondWhitePawnMove = "g4"
    val blackQueenMove      = "Qh4#"
    foolMateGame = foolMateGame(firstWhitePawnMove).toOption.value
    foolMateGame = foolMateGame(blackPawnMove).toOption.value
    foolMateGame = foolMateGame(secondWhitePawnMove).toOption.value
    foolMateGame.isKingInCheck shouldBe false
    foolMateGame = foolMateGame(blackQueenMove).toOption.value
    foolMateGame.isKingInCheck shouldBe true
    foolMateGame.gameStatus should equal(Win(Black))
  }

  /*
   * SCHOLAR'S MATE
   * */
  "Build a Scholar's Check Mate in which the game " should " end in 7 turn," in {
    val firstMoveWhitePawn    = "e4"
    val secondMoveBlackPawn   = "e5"
    val thirdMoveWhiteBishop  = "Bc4"
    val fourthMoveBlackKnight = "Nc6"
    val fifthMoveWhiteQueen   = "Qh5"
    val sixthMoveBlackKnight  = "Nf6"
    val seventhMoveWhiteQueen = "Qxf7#"
    scholarMateGame = scholarMateGame(firstMoveWhitePawn).toOption.value
    scholarMateGame = scholarMateGame(secondMoveBlackPawn).toOption.value
    scholarMateGame = scholarMateGame(thirdMoveWhiteBishop).toOption.value
    scholarMateGame = scholarMateGame(fourthMoveBlackKnight).toOption.value
    scholarMateGame = scholarMateGame(fifthMoveWhiteQueen).toOption.value
    scholarMateGame = scholarMateGame(sixthMoveBlackKnight).toOption.value
    scholarMateGame = scholarMateGame(seventhMoveWhiteQueen).toOption.value
    scholarMateGame.gameStatus should equal(Win(White))
    scholarMateGame.isKingInCheck shouldBe true
  }

}
