package it.scalachess.core.pieces

import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, Color, White }

final case class Piece(color: Color, pieceType: PieceType) {

  def canAttack(start: Position, end: Position): Boolean = {
    val colD: Int = start colDistance end
    pieceType match {
      case King   => start isAdjacentTo end
      case Queen  => (start isDiagonalTo end) || (start isStraightTo end)
      case Rook   => start isStraightTo end
      case Bishop => start isDiagonalTo end
      case Knight =>
        val rowD: Int = start rowDistance end
        (rowD == 2 && colD == 1) || (rowD == 1 && colD == 2)
      case Pawn =>
        val realRowD = end effectiveRowDistance start
        (color == White && realRowD == 1 && colD == 1 || color == Black && realRowD == -1 && colD == 1)
    }
  }

  def canMove(start: Position, end: Position): Boolean =
    pieceType match {
      case Pawn =>
        val orientedRowD = end effectiveRowDistance start
        val colD: Int    = start colDistance end
        (color == White && orientedRowD == 1 && colD == 0 || color == Black && orientedRowD == -1 && colD == 0)
      case _ => canAttack(start, end)
    }

  lazy val symbol: String = pieceType.symbol(color)
}
