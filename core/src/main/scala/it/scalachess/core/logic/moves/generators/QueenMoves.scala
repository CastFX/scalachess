package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.Queen

private[generators] object QueenMoves extends PieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] =
    (BishopMoves(color, board, from) ++ RookMoves(color, board, from))
      .map(x => ValidSimpleMove(x.from, x.to, Queen, x.color, x.capture))
}
