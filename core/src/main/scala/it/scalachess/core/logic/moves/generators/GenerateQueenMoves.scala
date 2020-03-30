package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType

case class GenerateQueenMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  override def apply(): List[ValidMove] =
    List() ++
    generateQueenMoves(from, 0, 1) ++
    generateQueenMoves(from, 0, -1) ++
    generateQueenMoves(from, 1, 0) ++
    generateQueenMoves(from, -1, 0) ++
    generateQueenMoves(from, 1, 1) ++
    generateQueenMoves(from, -1, -1) ++
    generateQueenMoves(from, 1, -1) ++
    generateQueenMoves(from, -1, 1)

  private def generateQueenMoves(from: Position, colMod: Int, rowMod: Int): List[ValidMove] =
    generateLinearMovement(pieceType,
                           color,
                           board,
                           from,
                           Position.of(from.col + colMod, from.row + rowMod),
                           "queen",
                           colMod,
                           rowMod,
                           List())

}
