package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelZero {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def generateMove(whiteAI: AI, blackAI: AI) {
    it should "generates a legal move in a default board" in {
      Given("an initial default board")
      val defaultBoard = Board.defaultBoard()
      val history = Seq() // the history doesn't matter in this test
      val whitePlayer = White
      val blackPlayer = Black

      When("one of the two A.I. generate a move")
      val whiteMove = whiteAI.generateSmartMove(defaultBoard, history)
      val blackMove = blackAI.generateSmartMove(defaultBoard, history)

      Then("the move should be contained in all the relative possible moves " +
        "(note: the A.I. doesn't know the concept of the turns or the fact that whites move first" +
        "those features are managed by the game)")
      val whiteMoves = new MoveGenerator(defaultBoard, whitePlayer, history).allMoves()
      val blackMoves = new MoveGenerator(defaultBoard, blackPlayer, history).allMoves()
      whiteMoves.contains(whiteMove) should be(true)
      blackMoves.contains(blackMove) should be(true)
    }
  }

}
