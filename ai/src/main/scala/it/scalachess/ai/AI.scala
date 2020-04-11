package it.scalachess.ai

import it.scalachess.ai.level.{ LevelOne, LevelThree, LevelTwo, LevelZero }
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

final case class AI(difficulty: Int) {

  private val level = difficulty match {
    case 0 => Some(LevelZero())
    case 1 => Some(LevelOne())
    case 2 => Some(LevelTwo())
    case 3 => Some(LevelThree())
    case _ => None
  }

  def generateSmartMove(board: Board, player: Color, history: Seq[FullMove]): Option[FullMove] = level match {
    case Some(level) => Some(level(board, player, history))
    case None        => None
  }

}
