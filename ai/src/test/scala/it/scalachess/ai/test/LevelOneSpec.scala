package it.scalachess.ai.test

import it.scalachess.ai.AI
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.{ Pawn, Queen }

class LevelOneSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  /*
   * simulate a FOOL'S MATE
   * */
  "A level one chess A.I during a Fool's Mate" should "plays the move which capture king" in {
    val firstWhitePawnMove  = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
    val blackPawnMove       = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
    val blackQueenMove      = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    val whiteKingPosition   = Position(5, 1)
    var board               = Board.defaultBoard()
    val ai                  = AI(1, Black)
    board = board(firstWhitePawnMove.boardChanges)
    board = board(blackPawnMove.boardChanges)
    board = board(secondWhitePawnMove.boardChanges)
    // ai.generateSmartMove(board, Seq()) // note: at this point, the AI could generate the checkmate move: 1 possibility over 30 (moves generated)
    board = board(blackQueenMove.boardChanges)
    ai.generateSmartMove(board, Seq()).toOption.value.validMove.capture.value should equal(whiteKingPosition)
  }

  // TODO maybe add one test where the ai must choose to capture a piece between two having different values

}
