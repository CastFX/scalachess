package it.scalachess.core.pieces

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.{ Black, Color, White }
import it.scalachess.core.logic.{ MoveValidator, ValidMove }

import scala.annotation.tailrec

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

  def allPossibleValidMove(start: Position, moveValidator: MoveValidator): Set[ValidMove] = {

    def allRookValidMoves: Set[ValidMove] = {
      val colRightEndPoint              = Position(Board.width, start.row)
      val colLeftEndPoint               = Position(1, start.row)
      val rowUpEndPoint                 = Position(start.col, Board.height)
      val rowDownEndPoint               = Position(start.col, 1)
      val allPossiblePos: Set[Position] = Set()
      start.generatePosBetweenCol(colRightEndPoint, allPossiblePos) + colRightEndPoint ++
      start.generatePosBetweenCol(colLeftEndPoint, allPossiblePos) + colLeftEndPoint ++
      start.generatePosBetweenRow(rowUpEndPoint, allPossiblePos) + rowUpEndPoint ++
      start.generatePosBetweenRow(rowDownEndPoint, allPossiblePos) + rowDownEndPoint
      allPossiblePos.flatMap(pos => moveValidator.validateMove(start, pos, color))
    }

    def allBishopValidMoves(pos: Position, positions: Set[Position]): Set[ValidMove] = {
      @tailrec
      def bishopWalkablePosInOneDirection(pos: Position,
                                          positions: Set[Position],
                                          rowMod: Int,
                                          colMod: Int): Set[Position] =
        if (pos.row > Board.height || pos.col > Board.width || pos.row < 1 || pos.col < 1)
          positions
        else
          bishopWalkablePosInOneDirection(Position(pos.col + colMod, pos.row + rowMod), positions + pos, rowMod, colMod)

      (bishopWalkablePosInOneDirection(start, positions, 1, 1) ++
      bishopWalkablePosInOneDirection(start, positions, -1, -1) ++
      bishopWalkablePosInOneDirection(start, positions, 1, -1) ++
      bishopWalkablePosInOneDirection(start, positions, -1, 1))
        .flatMap(pos => moveValidator.validateMove(start, pos, color))
    }

    pieceType match {
      case Knight =>
        (Set() + Position(start.col + 1, start.row + 2) + Position(start.col - 1, start.row + 2) +
        Position(start.col + 1, start.row - 2) + Position(start.col - 1, start.row - 2) +
        Position(start.col + 2, start.row + 1) + Position(start.col + 2, start.row - 1) +
        Position(start.col - 2, start.row + 1) + Position(start.col - 2, start.row - 1))
          .flatMap(pos => moveValidator.validateMove(start, pos, color))
      case Pawn =>
        color match {
          case White =>
            (Set() + Position(start.col, start.row + 1) + Position(start.col, start.row + 2) +
            Position(start.col + 1, start.row + 1) + Position(start.col - 1, start.row + 1))
              .flatMap(pos => moveValidator.validateMove(start, pos, color))
          case Black =>
            (Set() + Position(start.col, start.row - 1) + Position(start.col, start.row - 2) +
            Position(start.col + 1, start.row - 1) + Position(start.col - 1, start.row - 1))
              .flatMap(pos => moveValidator.validateMove(start, pos, color))
        }
      case King   => start.adjacentPositions.flatMap(pos => moveValidator.validateMove(start, pos, color))
      case Rook   => allRookValidMoves
      case Bishop => allBishopValidMoves(start, Set())
      case Queen  => allRookValidMoves ++ allBishopValidMoves(start, Set())
    }
  }

}
