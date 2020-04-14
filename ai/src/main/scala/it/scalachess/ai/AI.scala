package it.scalachess.ai

import it.scalachess.ai.level.{ LevelOne, LevelTwo, LevelZero }
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import scalaz.{ Failure, Validation }

final case class AI(difficulty: Int, player: Color) {

  private val level = {
    require(difficulty >= 0 && difficulty <= 2, "The AI difficulty inserted doesn't exists")
    difficulty match {
      case 0 => Some(LevelZero())
      case 1 => Some(LevelOne())
      case 2 => Some(LevelTwo())
      case _ => None
    }
  }

  def generateSmartMove(board: Board, history: Seq[FullMove]): Validation[String, FullMove] = level match {
    case Some(level) => level(board, player, history)
    case None        => Failure("Something went wrong during level initialization ...")
  }

}
