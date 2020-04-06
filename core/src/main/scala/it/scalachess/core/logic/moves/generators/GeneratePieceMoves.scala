package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove

private[generators] trait GeneratePieceMoves extends ((Color, Board, Position) => List[ValidMove]) {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove]
}
