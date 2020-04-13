package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.core.White
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MoveGenerator
import org.scalatest.{ FlatSpec, OptionValues }

class LevelZeroSpec extends FlatSpec with OptionValues {

  "A level zero chess A.I" should "generate a random move" in {
    val defaultBoard = Board.defaultBoard()
    val player       = White
    val history      = Seq()
    val ai           = AI(0, player)
    println(ai.generateSmartMove(defaultBoard, history).value)
    new MoveGenerator(defaultBoard, player, history)
      .allMoves()
      .contains(ai.generateSmartMove(defaultBoard, history).value)
  }

}
