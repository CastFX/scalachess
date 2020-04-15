package it.scalachess.ai

import it.scalachess.ai.level.{ LevelOne, LevelTwo, LevelZero }
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The class representing the logic of the AI player.
 * @param difficulty the AI level
 * @param player the color of the AI player
 */
final case class AI(difficulty: Int, player: Color) {

  private val level = {
    require(difficulty >= 0 && difficulty <= AI.maxDifficulty, "The AI difficulty inserted doesn't exists")
    difficulty match {
      case 0 => Some(LevelZero())
      case 1 => Some(LevelOne())
      case 2 => Some(LevelTwo())
      case _ => None
    }
  }

  def generateSmartMove(board: Board, history: Seq[FullMove]): FullMove = level match {
    case Some(level) => level(board, player, history)
  }
}

object AI {
  val maxDifficulty: Int = 2
}
