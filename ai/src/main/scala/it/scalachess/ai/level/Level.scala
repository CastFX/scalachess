package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import scalaz.Validation

trait Level extends ((Board, Color, Seq[FullMove]) => Validation[String, FullMove]) {
  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove]
}
