package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.core.White
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MoveGenerator
import org.scalatest.{ FlatSpec, OptionValues }

class LevelOneSpec extends FlatSpec with OptionValues {

  "A simple level one chess A.I" should "generate a ??? " in {
    val defaultBoard = Board.defaultBoard()
    val player       = White
    val history      = Seq()
    val ai           = AI(0)

  }

}
