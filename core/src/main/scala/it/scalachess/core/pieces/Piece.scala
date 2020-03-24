package it.scalachess.core.pieces

import it.scalachess.core.board.{ Position }
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
        val rowDistance = end rowDistance start
        (color == White && rowDistance == 1 && colDistance == 1
        || color == Black && rowDistance == -1 && colDistance == 1)
    }
  }

  def canMove(start: Position, end: Position): Boolean =
    pieceType match {
      case Pawn =>
        val rowDistanceInt = end rowDistance start
        val colDistanceAbs = start colDistanceAbs end
        (color == White && rowDistanceInt == 1 && colDistanceAbs == 0
        || color == Black && rowDistanceInt == -1 && colDistanceAbs == 0
        || (start.row == 2 && color == White && rowDistanceInt == 2 && colDistanceAbs == 0)
        || (start.row == 7 && color == Black && rowDistanceInt == -2 && colDistanceAbs == 0))
      case _ => canAttack(start, end)
    }

  /*
  // TODO working on CheckMate ....
  def computeAllPossibleMoves(from: Position, moveValidator: MoveValidator): Set[ValidMove] = {
    def extractValidMove(validMove: Validation[String, ValidMove]): Option[ValidMove] =
      validMove match {
        case Success(move) => Some(move)
        case _             => None
      }
    def computeRookValidMoves: Set[ValidMove] = {
      val colRightEndPoint = Position(Board.width, from.row)
      val colLeftEndPoint  = Position(1, from.row)
      val rowUpEndPoint    = Position(from.col, Board.height)
      val rowDownEndPoint  = Position(from.col, 1)
      val allPossiblePos: Set[Position] = Set() ++
      from.computePosBetweenCol(colRightEndPoint, allPossiblePos) + colRightEndPoint ++
      from.computePosBetweenCol(colLeftEndPoint, allPossiblePos) + colLeftEndPoint ++
      from.computePosBetweenRow(rowUpEndPoint, allPossiblePos) + rowUpEndPoint ++
      from.computePosBetweenRow(rowDownEndPoint, allPossiblePos) + rowDownEndPoint
      allPossiblePos.flatMap(pos => extractValidMove(moveValidator.validate(from, pos, color)))
    }
    pieceType match {
      case Knight => ???
      case Pawn   => ???
      case King   => ???
      case Queen  => ???
      case Rook   => computeRookValidMoves
      case Bishop => ???
    }
  }
 */
}
