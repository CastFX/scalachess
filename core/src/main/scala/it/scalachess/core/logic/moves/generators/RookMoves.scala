package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.Rook

private[generators] object RookMoves extends PieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] = {
    val pos = List(1, -1, 0)
    val result = for {
      a <- pos
      b <- pos
      if math.abs(a) != math.abs(b)
    } yield linearMovement(Rook, color, board, from, Position.of(from.col + a, from.row + b), a, b)
    result.flatten
  }
}
