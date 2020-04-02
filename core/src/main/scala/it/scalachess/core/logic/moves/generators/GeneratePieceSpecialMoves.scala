package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

private[generators] trait GeneratePieceSpecialMoves
    extends ((PieceType, Color, Board, Position, List[ValidMove]) => List[ValidMove]) {
  override def apply(pieceType: PieceType,
                     color: Color,
                     board: Board,
                     from: Position,
                     history: List[ValidMove]): List[ValidMove]
}
