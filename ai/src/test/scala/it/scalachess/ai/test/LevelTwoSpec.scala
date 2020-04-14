package it.scalachess.ai.test

import java.util.Calendar

import it.scalachess.ai.AI
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.{ Pawn, Queen }

class LevelTwoSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  /*
   * simulate a FOOL'S MATE
   * */
  "A level two chess A.I during a Fool's Mate" should "plays the move which cause checkmate" in {
    val firstWhitePawnMove  = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
    val blackPawnMove       = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
    val blackQueenMove      = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    val whiteKingPosition   = Position(5, 1)
    var board               = Board.defaultBoard()
    val ai                  = AI(2, Black)
    board = board(firstWhitePawnMove.boardChanges)
    //TODO verificare che non genera la mossa del fool's mate perchè pensa che l'avversario non sia un fool
    //TODO però potrebbe generarla nel caso in cui i due pedini bianchi si siano già mossi
    board = board(blackPawnMove.boardChanges)
    board = board(secondWhitePawnMove.boardChanges)
    // move that cause checkmate
    val start  = Calendar.getInstance().getTime.getTime
    val aiMove = ai.generateSmartMove(board, Seq()).toOption.value
    val end    = Calendar.getInstance().getTime.getTime
    println(start - end) // minimax non migliorato ci mette dai: 4.2 ai 5.6 secondi
    aiMove.resultsInCheckmate should be(true)
    aiMove.validMove should equal(blackQueenMove)
    board = board(aiMove.validMove.boardChanges)
    // move that capture the king
    ai.generateSmartMove(board, Seq()).toOption.value.validMove.capture.value should equal(whiteKingPosition)
  }

  // todo testare la gen dello scacco scholar perchè in teoria dovrebbe funzionare solo con profondità 3 del minimax

}
