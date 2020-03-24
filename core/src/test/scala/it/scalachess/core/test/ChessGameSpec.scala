package it.scalachess.core.test

import it.scalachess.core.ChessGame
import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, White }
import it.scalachess.core.pieces.{ Pawn, Piece }
import org.scalatest.{ FlatSpec, Matchers }
import it.scalachess.core.test.ChessGameFailureMatcher.generateFailure

class ChessGameSpec extends FlatSpec with Matchers {
  var simpleGame            = ChessGame.standard()
  var foolMateGame          = ChessGame.standard()
  var simpleGameTurnCounter = 0

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

  it should "not allows players to execute an illegal move" in {
    val pawnMovesForThreePos = "a2 a5"
    simpleGame(pawnMovesForThreePos) should generateFailure
  }

  it should "not allows a player captures his own pieces" in {
    val queenCapturesPawn = "d1 d2"
    simpleGame(queenCapturesPawn) should generateFailure
  }

  it should "not allows specific pieces pass throught other pieces" in {
    val queenPassThroughPawn = "d1 d3"
    simpleGame(queenPassThroughPawn) should generateFailure
  }

  it should "not allows move a piece in the same position" in {
    val queenMoveStandFirm = "d1 d1"
    simpleGame(queenMoveStandFirm) should generateFailure
  }

  it should "apply a move correctly" in {
    val whitePawnMove          = "a2 a3"
    val whitePawnFinalPosition = Position(1, 3)
    simpleGame(whitePawnMove).toOption.get.board.pieceAtPosition(whitePawnFinalPosition).get should equal(
      Piece(White, Pawn))
  }

  it should "must alternate turns between players" in {
    val whitePawnMove = "a2 a3"
    val blackPawnMove = "a7 a6"
    simpleGame = simpleGame(whitePawnMove).toOption.get
    simpleGameTurnCounter += 1
    simpleGame.turn should be(simpleGameTurnCounter)
    simpleGame.player should equal(Black)
    simpleGame = simpleGame(blackPawnMove).toOption.get
    simpleGameTurnCounter += 1
    simpleGame.turn should be(simpleGameTurnCounter)
    simpleGame.player should equal(White)
  }
  /*
   * FOOL'S CHECK MATE // TODO is going to be a Checkmate but currently the program realizes only check
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val firstWhitePawnMove   = "f2 f3"
    val secondBlackPawnMove  = "e7 e6"
    val thirdWhitePawnMove   = "g2 g4"
    val fourthBlackQueenMove = "d8 h4"
    foolMateGame = foolMateGame(firstWhitePawnMove).toOption.get
    foolMateGame = foolMateGame(secondBlackPawnMove).toOption.get
    foolMateGame = foolMateGame(thirdWhitePawnMove).toOption.get
    foolMateGame.isKingInCheck should be(false)
    foolMateGame = foolMateGame(fourthBlackQueenMove).toOption.get
    foolMateGame.isKingInCheck should be(true)
  }

}
