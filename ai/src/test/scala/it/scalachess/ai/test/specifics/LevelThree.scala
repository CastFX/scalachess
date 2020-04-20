package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces._
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelThree {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def generateLastFoolsMateMove(blackAI: AI) {
    it should "generate the Fool's mate" in {
      info("To generate a checkmate, it's necessary a minimax depth = 2")
      Given("a board with almost the Fool's mate built, except the last move")
      val firstWhitePawnMove = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
      val blackPawnMove = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
      val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
      val blackQueenMove = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      board = board(firstWhitePawnMove.boardChanges)
      board = board(blackPawnMove.boardChanges)
      board = board(secondWhitePawnMove.boardChanges)

      When("the black A.I. generates the move")
      val blackAIMove = blackAI.apply(board, history)

      Then("the move should be last Fool's mate move, and cause checkmate")
      blackAIMove.validMove should be(blackQueenMove)
      blackAIMove.resultsInCheckmate should be(true)
    }
  }

  def generateLastScholarsMateMove(whiteAI: AI) {
    it should "generate the Scholar's mate" in {
      info("To generate a checkmate, it's necessary a minimax depth = 2")
      Given("a board with almost the Scholar's mate built, except the last move")
      val firstMoveWhitePawn = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
      val secondMoveBlackPawn = ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None)
      val thirdMoveWhiteBishop = ValidSimpleMove(Position(6, 1), Position(3, 4), Bishop, White, None)
      val fourthMoveBlackKnight = ValidSimpleMove(Position(2, 8), Position(3, 6), Knight, Black, None)
      val fifthMoveWhiteQueen = ValidSimpleMove(Position(4, 1), Position(8, 5), Queen, White, None)
      val sixthMoveBlackKnight = ValidSimpleMove(Position(7, 8), Position(6, 6), Knight, Black, None)
      val seventhMoveWhiteQueen = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, Some(Position(6, 7)))
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      board = board(firstMoveWhitePawn.boardChanges)
      board = board(secondMoveBlackPawn.boardChanges)
      board = board(thirdMoveWhiteBishop.boardChanges)
      board = board(fourthMoveBlackKnight.boardChanges)
      board = board(fifthMoveWhiteQueen.boardChanges)
      board = board(sixthMoveBlackKnight.boardChanges)

      When("the white A.I. generates the move")
      val whiteAIMove = whiteAI.apply(board, history)
      Then("the move should be last Scholar's mate move, and cause checkmate")
      whiteAIMove.validMove should be(seventhMoveWhiteQueen)
      whiteAIMove.resultsInCheckmate should be(true)
    }
  }

}