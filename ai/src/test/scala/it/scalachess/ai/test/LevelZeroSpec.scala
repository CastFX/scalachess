package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.ai.level.Level
import it.scalachess.core.{Black, White}
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{Pawn, Queen}
import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

class LevelZeroSpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("the chess A.I. (not matter about level) can't be used in a checkmate situation") {

    info("the chess A.I.")
    info("refuses to play a game that's already ended")

    scenario("A.I. raises exception because the game is ended") {

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

      Given("the two A.I. representing the two players")
      val whiteAI = AI(0, White)
      val blackAI = AI(0, Black)

      When("the white A.I. in checkmate tries to generate the move")
      var whiteAIResult = "success"
      try {
        whiteAI.generateSmartMove(board, history)
      } catch {
        case e: IllegalArgumentException => whiteAIResult = e.getMessage
      }
      Then("the white A.I. raises the relative exception")
      whiteAIResult should be("requirement failed: " + Level.aiIsInCheckmateErrorMsg)
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

    feature("the level zero chess A.I. plays a random move") {

      scenario("It generates a random move in a default board") {

        Given("an initial default board")
        val defaultBoard = Board.defaultBoard()
        val history = Seq()
        val whitePlayer = White
        val blackPlayer = Black

        Given("the two A.I. representing the two players")
        val whiteAI = AI(0, whitePlayer)
        val blackAI = AI(0, blackPlayer)

        When("one of the two A.I. generate a move")
        val whiteMove = whiteAI.generateSmartMove(defaultBoard, history)
        val blackMove = blackAI.generateSmartMove(defaultBoard, history)

        Then("the move should be contained in all the relative possible moves " +
          "(note: the A.I. doesn't know the concept of the turns or the fact that whites move first")
        val whiteMoves = new MoveGenerator(defaultBoard, whitePlayer, history).allMoves()
        val blackMoves = new MoveGenerator(defaultBoard, blackPlayer, history).allMoves()
        whiteMoves.contains(whiteMove) should be(true)
        blackMoves.contains(blackMove) should be(true)
      }
  }



}
