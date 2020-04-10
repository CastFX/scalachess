package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.ValidMove

private[Level] trait Level {
  def apply(board: Board, color: Color): ValidMove
}
