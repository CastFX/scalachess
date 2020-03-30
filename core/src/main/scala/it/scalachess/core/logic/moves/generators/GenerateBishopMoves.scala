package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

case class GenerateBishopMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  override def apply(): List[ValidMove] =
    List() ++
    generateBishopMoves(from, 1, 1) ++
    generateBishopMoves(from, -1, -1) ++
    generateBishopMoves(from, 1, -1) ++
    generateBishopMoves(from, -1, 1)

  private def generateBishopMoves(from: Position, colMod: Int, rowMod: Int): List[ValidMove] =
    generateLinearMovement(pieceType,
                           color,
                           board,
                           from,
                           Position.of(from.col + colMod, from.row + rowMod),
                           "bishop",
                           colMod,
                           rowMod,
                           List())

}
