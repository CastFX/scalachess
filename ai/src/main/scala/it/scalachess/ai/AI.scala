package it.scalachess.ai

import it.scalachess.ai.level.{Level, LevelOne, LevelThree, LevelTwo, LevelZero}
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The class representing the logic of the AI player.
 * @param difficulty the AI level
 * @param player the color of the AI player
 */
final case class AI(difficulty: Int, player: Color) {

  private val level: Level = {
    require(difficulty >= 0 && difficulty <= AI.maxDifficulty, "The AI difficulty inserted doesn't exists")
    difficulty match {
      case 0 => new LevelZero()
      case 1 => new LevelOne()
      case 2 => new LevelTwo()
      case 3 => new LevelThree()
    }
  }

  def generateSmartMove(board: Board, history: Seq[FullMove]): FullMove = level(board, player, history)

}

object AI {
  val maxDifficulty: Int = 2
}
