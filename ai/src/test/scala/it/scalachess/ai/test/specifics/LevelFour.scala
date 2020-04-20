package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.Board
import it.scalachess.core.pieces.Knight
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelFour {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def generateKnightMoveAtTheStart(whiteAI: AI): Unit = {
    it should "generate as first move one where a knight is moved, because the position value is more convenient" in {
      val defaultBoard = Board.defaultBoard()
      val history = Seq() // the history doesn't matter in this test
      val whiteMove = whiteAI.generateSmartMove(defaultBoard, history)
      whiteMove.validMove.pieceType should be(Knight)
    }
  }

}