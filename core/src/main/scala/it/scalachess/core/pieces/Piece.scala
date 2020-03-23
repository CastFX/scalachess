package it.scalachess.core.pieces

import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, Color, White }

final case class Piece(color: Color, pieceType: PieceType) {

  def canAttack(start: Position, end: Position): Boolean = {
    val colDistance = start colDistanceAbs end
    pieceType match {
      case King   => start isAdjacentTo end
      case Queen  => (start isDiagonalTo end) || (start isStraightTo end)
      case Rook   => start isStraightTo end
      case Bishop => start isDiagonalTo end
      case Knight =>
        val rowDistance = start rowDistanceAbs end
        (rowDistance == 2 && colDistance == 1
        || rowDistance == 1 && colDistance == 2)
      case Pawn =>
        val rowDistance = end rowDistanceInt start
        (color == White && rowDistance == 1 && colDistance == 1
        || color == Black && rowDistance == -1 && colDistance == 1)
    }
  }

  def canMove(start: Position, end: Position): Boolean =
    pieceType match {
      case Pawn =>
        val rowDistanceInt = end rowDistanceInt start
        val colDistanceAbs = start colDistanceAbs end
        (color == White && rowDistanceInt == 1 && colDistanceAbs == 0
        || color == Black && rowDistanceInt == -1 && colDistanceAbs == 0
        || (start.row == 2 && color == White && rowDistanceInt == 2 && colDistanceAbs == 0)
        || (start.row == 7 && color == Black && rowDistanceInt == -2 && colDistanceAbs == 0))
      case _ => canAttack(start, end)
    }

  lazy val symbol: String = pieceType.symbol(color)
}
