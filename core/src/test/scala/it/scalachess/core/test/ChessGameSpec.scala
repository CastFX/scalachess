package it.scalachess.core.test

import it.scalachess.core.ChessGame
import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, White }
import it.scalachess.core.pieces.{ Pawn, Piece }
import org.scalatest.{ FlatSpec, Matchers }
import scalaz.{ Failure, Success, Validation }

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
   * FOOL'S MATE
   * */

  val firstWhitePawnMove         = "f2 f3"
  val firstWhitePawnPosAfterMove = Position(6, 3)
  "A Pawn" should "be able to move one position forward" in {
    game = game.moveAttempt(firstWhitePawnMove)
    assert(game.board.pieceAtPosition(firstWhitePawnPosAfterMove).get == Piece(White, Pawn))
    //assert(game.moveAftermath() != Left("Game End"))
    //assert(game.moveAftermath() != Left("The player is under check"))
  }
  val secondBlackPawnMove = "e7 e6"
  "After the first successfull move, the black faction" should "takes turn" in {
    turnCounter += 1
    assert(game.player === Black)
    assert(game.turn == turnCounter)
    game = game.moveAttempt(secondBlackPawnMove)
    //assert(game.moveAftermath() != Left("Game End"))
    //assert(game.moveAftermath() != Left("The player is under check"))
  }
  val thirdWhitePawnMove  = "g2 g3"
  val fourthBlackPawnMove = "e6 e5"
  val fifthWhitePawnMove  = "g3 g4"
  val sixthBlackQueenMove = "d8 h4"
  "Now keep move the pieces following the Fool's checkmate procedure" should "cause checkmate" in {
    game = game.moveAttempt(thirdWhitePawnMove)
    //assert(game.moveAftermath() != Left("Game End"))
    //assert(game.moveAftermath() != Left("The player is under check"))
    game = game.moveAttempt(fourthBlackPawnMove)
    //assert(game.moveAftermath() != Left("Game End"))
    //assert(game.moveAftermath() != Left("The player is under check"))
    game = game.moveAttempt(fifthWhitePawnMove)
    //assert(game.moveAftermath() != Left("Game End"))
    //assert(game.moveAftermath() != Left("The player is under check"))
    game = game.moveAttempt(sixthBlackQueenMove)
    assert(game.moveAftermath() == Left("Game End"))
    //assert(game.moveAftermath() == Left("The player is under check"))
  }

}
