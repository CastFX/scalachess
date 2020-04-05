package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidMove }

private[generators] trait GeneratePieceSpecialMoves
    extends ((Color, Board, Position, Seq[FullMove]) => List[ValidMove]) {
  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove]
}
