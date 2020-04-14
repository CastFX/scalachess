package it.scalachess.ai.test

import it.scalachess.ai.AI
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.{ Bishop, Knight, Pawn, Queen }

class LevelTwoSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  /*
   * simulate a FOOL'S MATE
   * */
  "A level two chess A.I during a Fool's Mate" should "plays the move which cause checkmate" in {
    val firstWhitePawnMove  = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
    val blackPawnMove       = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
    val blackQueenMove      = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    var board               = Board.defaultBoard()
    val ai                  = AI(2, Black)
    board = board(firstWhitePawnMove.boardChanges)
    board = board(blackPawnMove.boardChanges)
    board = board(secondWhitePawnMove.boardChanges)
    // move that cause checkmate
    val aiMove = ai.generateSmartMove(board, Seq())
    aiMove.resultsInCheckmate should be(true)
    aiMove.validMove should equal(blackQueenMove)
    board = board(aiMove.validMove.boardChanges)
  }

  /*
   * simulate a SCHOLAR'S MATE
   * */
  "A level two chess A.I during a Scholar's Mate" should "plays the move which cause checkmate" in {
    val firstMoveWhitePawn    = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
    val secondMoveBlackPawn   = ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None)
    val thirdMoveWhiteBishop  = ValidSimpleMove(Position(6, 1), Position(3, 4), Bishop, White, None)
    val fourthMoveBlackKnight = ValidSimpleMove(Position(2, 8), Position(3, 6), Knight, Black, None)
    val fifthMoveWhiteQueen   = ValidSimpleMove(Position(4, 1), Position(8, 5), Queen, White, None)
    val sixthMoveBlackKnight  = ValidSimpleMove(Position(7, 8), Position(6, 6), Knight, Black, None)
    val seventhMoveWhiteQueen = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, Some(Position(6, 7)))
    var board                 = Board.defaultBoard()
    val ai                    = AI(2, White)
    board = board(firstMoveWhitePawn.boardChanges)
    board = board(secondMoveBlackPawn.boardChanges)
    board = board(thirdMoveWhiteBishop.boardChanges)
    board = board(fourthMoveBlackKnight.boardChanges)
    board = board(fifthMoveWhiteQueen.boardChanges)
    board = board(sixthMoveBlackKnight.boardChanges)
    // move that cause checkmate
    val aiMove = ai.generateSmartMove(board, Seq())
    aiMove.validMove should equal(seventhMoveWhiteQueen)
  }

}
