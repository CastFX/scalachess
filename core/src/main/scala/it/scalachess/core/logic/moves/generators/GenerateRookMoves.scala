package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType

case class GenerateRookMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  override def apply(): Set[ValidMove] =
    Set() ++
    generateRookMoves(from, 0, 1) ++
    generateRookMoves(from, 0, -1) ++
    generateRookMoves(from, 1, 0) ++
    generateRookMoves(from, colMod = -1, rowMod = 0)

  private def generateRookMoves(from: Position, colMod: Int, rowMod: Int): List[ValidMove] =
    generateLinearMovement(pieceType,
                           color,
                           board,
                           from,
                           Position.of(from.col + colMod, from.row + rowMod),
                           "rook",
                           colMod,
                           rowMod,
                           List())

}
