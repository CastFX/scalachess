package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.ai.level.Level
import it.scalachess.core.{Black, White}
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.{Pawn, Queen}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait WrongUsage {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def chessAICantBeUsedDuringCheckmate(whiteAI: AI, blackAI: AI) {
    "The chess A.I. (doesn't matter about level)" should "not be used when the game is ended" in {
      Given("a board with the white player in checkmate (Fool's mate)")
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      val firstWhitePawnMove = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
      val blackPawnMove = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
      val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
      val blackQueenMove = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
      board = board(firstWhitePawnMove.boardChanges)
      board = board(blackPawnMove.boardChanges)
      board = board(secondWhitePawnMove.boardChanges)
      board = board(blackQueenMove.boardChanges)

      When("the white A.I. in checkmate tries to generate the move")
      var whiteAIComputationResult = "success"
      try {
        whiteAI.generateSmartMove(board, history)
      } catch {
        case e: IllegalArgumentException => whiteAIComputationResult = e.getMessage
      }
      Then("the white A.I. raises the relative exception")
      whiteAIComputationResult should be("requirement failed: " + Level.aiIsInCheckmateErrorMsg)
      And("the game will crash because it must determine the victory condition before this exception is throwed")

      When("the black A.I. tries to generate the move, against his opponent in checkmate")
      var blackAIResult = "success"
      try {
        blackAI.generateSmartMove(board, history)
      } catch {
        case e: IllegalArgumentException => blackAIResult = e.getMessage
      }
      Then("the white A.I. raises the relative exception")
      blackAIResult should be("requirement failed: " + Level.opponentIsInCheckmateErrorMsg)
      And("the game will crash because it must determine the victory condition before this exception is throwed")
    }
  }

}