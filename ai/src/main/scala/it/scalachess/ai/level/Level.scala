package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

private[ai] trait Level extends ((Board, Color, Seq[FullMove]) => FullMove) {
  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove
}
