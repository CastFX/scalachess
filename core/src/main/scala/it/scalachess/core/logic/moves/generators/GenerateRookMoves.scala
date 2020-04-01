package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

private[generators] object GenerateRookMoves extends GeneratePieceMoves {
  override def apply(pieceType: PieceType, color: Color, board: Board, from: Position): List[ValidMove] =
    generateLinearMovementSimpleMoves(pieceType, color, board, from, from.posUp, 0, from.rowUpMod, List()) ++
    generateLinearMovementSimpleMoves(pieceType, color, board, from, from.posDown, 0, from.rowDownMod, List()) ++
    generateLinearMovementSimpleMoves(pieceType, color, board, from, from.posRight, from.colRightMod, 0, List()) ++
    generateLinearMovementSimpleMoves(pieceType, color, board, from, from.posLeft, from.colLeftMod, 0, List())
}
