package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.Knight

private[generators] object KnightMoves extends PieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] = {
    val pos = List(1, -1, 2, -2)
    val result = for {
      a <- pos
      b <- pos
      if math.abs(a) != math.abs(b)
    } yield simpleMove(Knight, color, board, from, Position.of(from.col + a, from.row + b))
    result.flatten
  }
}
