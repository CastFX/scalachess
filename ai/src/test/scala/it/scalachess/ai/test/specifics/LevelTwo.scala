package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.{FullMove, ValidSimpleMove}
import it.scalachess.core.pieces._
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelTwo {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def willNotBeTrickedOnTheNextEnemyMove(whiteAI: AI): Unit = {
    it should "not be tricked very easily: it should not lose a queen to capture a pawn" in {
      info("The white player has only two pieces: the king and a queen")
      info("The black player has only three pieces: the king and two pawn")
      info("The kings are isolated: the white is in right down corner; the black is in the right up corner")
      info("One of the black pawn protect the other, and this last one is exposed to the white queen attack")
      info("To avoid this naive trap and have this test passed, it's necessary a minimax depth = 2")
      Given("the specific board previously described")
      val blackPawnExposedPosition = Position(5, 6)
      val whiteQueen = Piece(White, Queen)
      val blackPawn = Piece(Black, Pawn)
      val pieceMap = Map(Position(8, 1) -> Piece(White, King), Position(8, 8) -> Piece(Black, King),
        Position(3, 4) -> whiteQueen, blackPawnExposedPosition -> blackPawn, Position(6, 7) -> blackPawn)
      var board = Board(pieceMap)
      val history = Seq( // the history needs to contains a move for each king, to prevent the castling move generation
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      )

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board will still contains the black pawn which was exposed before")
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(blackPawn))
    }
  }

}