package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.{FullMove, ValidSimpleMove}
import it.scalachess.core.pieces._
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelThree {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def generateKnightMoveAtTheStart(whiteAI: AI): Unit = {
    it should "generate as first move one where a knight is moved, because the position value is more convenient" in {
      val defaultBoard = Board.defaultBoard()
      val history = Seq() // the history doesn't matter in this test
      val whiteMove = whiteAI.generateSmartMove(defaultBoard, history)
      whiteMove.validMove.pieceType should be (Knight)
    }
  }

}