package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

private[generators] object GenerateQueenMoves extends GeneratePieceMoves {
  override def apply(pieceType: PieceType, color: Color, board: Board, from: Position): List[ValidMove] =
    GenerateBishopMoves(pieceType, color, board, from) ++
    GenerateRookMoves(pieceType, color, board, from)
}
