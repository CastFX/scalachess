package it.scalachess.core.pieces

import it.scalachess.core.board.Position
import it.scalachess.core.colors.Color

case class Piece(color: Color, pieceType: PieceType) {

  def canAttack(start: Position, end: Position): Boolean = {
    val dx = start xDistanceTo end
    val dy = start yDistanceTo end
    pieceType match {
      case King   => start isAdjacentTo end
      case Queen  => (start isAdjacentTo end) || (start isStraightTo end)
      case Rook   => start isStraightTo end
      case Bishop => start isDiagonalTo end
      case Knight => (dx == 2 && dy == 1) || (dx == 1 && dy == 2)
      case Pawn   => Piece.canPawnAttack(start, end, color)
    }
  }

  def canMove(start: Position, end: Position): Boolean = pieceType match {
    case Pawn => Piece.canPawnMove(start, end, color)
    case _    => canAttack(start, end)
  }

  lazy val symbol: String = pieceType.symbol(color)
}

object Piece {
  private def canPawnAttack(start: Position, end: Position, color: Color): Boolean =
    true //TODO bondi
  private def canPawnMove(start: Position, end: Position, color: Color): Boolean =
    true //TODO bondi
}
