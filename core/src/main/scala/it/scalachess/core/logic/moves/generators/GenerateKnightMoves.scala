package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

private[generators] object GenerateKnightMoves extends GeneratePieceMoves {
  override def apply(pieceType: PieceType, color: Color, board: Board, from: Position): List[ValidMove] =
    List(
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col + 1, from.row + 2)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col - 1, from.row + 2)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col + 1, from.row - 2)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col - 1, from.row - 2)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col + 2, from.row + 1)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col + 2, from.row - 1)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col - 2, from.row + 1)),
      generateSimpleMove(pieceType, color, board, from, Position.of(from.col - 2, from.row - 1))
    ).filter(validation => validation.isSuccess)
      .map(_.toOption.get)
}
