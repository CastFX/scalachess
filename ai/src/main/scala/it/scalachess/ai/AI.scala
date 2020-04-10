package it.scalachess.ai

import it.scalachess.ai.level.{ Level, LevelOne, LevelThree, LevelTwo, LevelZero }
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }

final case class AI(difficulty: Int) {

  private val level = difficulty match {
    case 0 => LevelZero
    case 1 => LevelOne
    case 2 => LevelTwo
    case 3 => LevelThree
  }

  def apply(board: Board, player: Color): ValidMove = level match {
    case LevelOne => ???
  }

}
