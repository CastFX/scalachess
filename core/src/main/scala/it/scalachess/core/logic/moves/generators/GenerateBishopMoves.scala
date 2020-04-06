package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.Bishop

private[generators] object GenerateBishopMoves extends GeneratePieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] =
    generateLinearMovementSimpleMoves(Bishop,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colRightMod, from.row + from.rowUpMod),
                                      from.colRightMod,
                                      from.rowUpMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(Bishop,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colRightMod, from.row + from.rowDownMod),
                                      from.colRightMod,
                                      from.rowDownMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(Bishop,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colLeftMod, from.row + from.rowDownMod),
                                      from.colLeftMod,
                                      from.rowDownMod,
                                      List()) ++
    generateLinearMovementSimpleMoves(Bishop,
                                      color,
                                      board,
                                      from,
                                      Position.of(from.col + from.colLeftMod, from.row + from.rowUpMod),
                                      from.colLeftMod,
                                      from.rowUpMod,
                                      List())
}