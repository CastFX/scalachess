package it.scalachess.core.test

import it.scalachess.core.ChessGame
import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, White }
import it.scalachess.core.gamestatus.Ongoing
import it.scalachess.core.pieces.{ Pawn, Piece }
import org.scalatest.{ FlatSpec, Matchers }

class ChessGameSpec extends FlatSpec with Matchers {
  var game        = ChessGame.standard()
  var turnCounter = 0

  "A standard ChessGame" should "have a non-empty board" in {
    assert(game.board.pieces.nonEmpty)
  }

  it should "have a starting white player at turn 0" in {
    assert(game.player === White)
    assert(game.turn == turnCounter)
  }

  /*
   * FOOL'S CHECK MATE // TODO is going to be a Checkmate but currently the program realizes only check
   * */
  "Let's build a Fool's Check Mate in which the game end in 4 turn, it" should "move the firs Pawn" in {
    val firstWhitePawnMove         = "f2 f3"
    val firstWhitePawnPosAfterMove = Position(6, 3)
    game(firstWhitePawnMove).isSuccess should equal(true)
    this.game = game(firstWhitePawnMove).toOption.get
    this.game.board.pieceAtPosition(firstWhitePawnPosAfterMove).get should equal(Piece(White, Pawn))
    this.game.gameStatus should be(Ongoing)
    this.game.isKingInCheck should be(false)
  }
  it should ", after the first successfull move, increase the turnCounter " +
  "and switch the turn to Black faction" in {
    turnCounter += 1
    game.turn should equal(turnCounter)
    game.player should equal(Black)
    val secondBlackPawnMove = "e7 e6"
    game = game(secondBlackPawnMove).toOption.get
    game.gameStatus should be(Ongoing)
    game.isKingInCheck should be(false)
  }
  it should "keeps move the pieces following the Fool's checkmate procedure and cause a checkmate" in {
    val thirdWhitePawnMove   = "g2 g4"
    val fourthBlackQueenMove = "d8 h4"
    game = game(thirdWhitePawnMove).toOption.get
    game.gameStatus should be(Ongoing)
    game.isKingInCheck should be(false)
    game = game(fourthBlackQueenMove).toOption.get
    game.gameStatus should be(Ongoing)
    game.isKingInCheck should be(true)
  }

}
