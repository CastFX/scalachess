package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

private[generators] object GenerateBishopMoves extends GeneratePieceMoves {
  override def apply(pieceType: PieceType, color: Color, board: Board, from: Position): List[ValidMove] =
    generateLinearMovementSimpleMoves(pieceType,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colRightMod, from.row + from.rowUpMod),
                                      from.colRightMod,
                                      from.rowUpMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(pieceType,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colRightMod, from.row + from.rowDownMod),
                                      from.colRightMod,
                                      from.rowDownMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(pieceType,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colLeftMod, from.row + from.rowDownMod),
                                      from.colLeftMod,
                                      from.rowDownMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(pieceType,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colLeftMod, from.row + from.rowUpMod),
                                      from.colLeftMod,
                                      from.rowUpMod,
                                      List())
}
