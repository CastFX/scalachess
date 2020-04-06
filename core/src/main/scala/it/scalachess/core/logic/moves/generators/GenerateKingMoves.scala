package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.King

private[generators] object GenerateKingMoves extends GeneratePieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] =
    from.adjacentPositions
      .flatMap(to => generateSimpleMove(King, color, board, from, Position.of(to)).toOption)
      .toList
}