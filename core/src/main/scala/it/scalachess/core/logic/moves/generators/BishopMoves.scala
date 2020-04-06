package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.Bishop

private[generators] object BishopMoves extends PieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] = {
    val pos = List(1, -1)
    val result = for {
      a <- pos
      b <- pos
    } yield linearMovement(Bishop, color, board, from, Position.of(from.col + a, from.row + b), a, b)
    result.flatten
  }
}
