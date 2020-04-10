package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.ValidMove

case class LevelZero() extends Level {
  override def apply(board: Board, color: Color): ValidMove = ???
}
