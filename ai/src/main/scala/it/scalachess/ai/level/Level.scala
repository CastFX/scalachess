package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

trait Level {
  def generateSmartMove(board: Board, player: Color, history: Seq[FullMove]): FullMove
}
