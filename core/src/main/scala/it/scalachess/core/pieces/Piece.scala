package it.scalachess.core.pieces

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ MoveValidator, ValidMove }

import scala.annotation.tailrec

final case class Piece(color: Color, pieceType: PieceType) {

  lazy val symbol: String = pieceType.symbol(color)

  def canAttack(start: Position, end: Position): Boolean = {
    def knightCanAttack(): Boolean = {
      val rowDistance = start rowDistanceAbs end
      val colDistance = start colDistanceAbs end
      (rowDistance == 2 && colDistance == 1
      || rowDistance == 1 && colDistance == 2)
    }
    def pawnCanAttack(): Boolean = {
      val rowDistance = end rowDistance start
      val colDistance = start colDistanceAbs end
      (color == White && rowDistance == 1 && colDistance == 1
      || color == Black && rowDistance == -1 && colDistance == 1)
    }
    pieceType match {
      case King   => start isAdjacentTo end
      case Queen  => (start isDiagonalTo end) || (start isStraightTo end)
      case Rook   => start isStraightTo end
      case Bishop => start isDiagonalTo end
      case Knight => knightCanAttack()
      case Pawn   => pawnCanAttack()
    }
  }

  def canMove(start: Position, end: Position): Boolean = {
    def pawnCanMove(): Boolean = {
      val rowDistanceInt = end rowDistance start
      val colDistanceAbs = start colDistanceAbs end
      color match {
        case White =>
          rowDistanceInt == 1 && colDistanceAbs == 0 || start.row == Board.whitePawnsStartingRow && rowDistanceInt == 2 && colDistanceAbs == 0
        case Black =>
          val rowDistanceAfterMove      = -1
          val rowDistanceAfterFirstMove = -2
          (rowDistanceInt == rowDistanceAfterMove && colDistanceAbs == 0
          || start.row == Board.blackPawnStartingRow && rowDistanceInt == rowDistanceAfterFirstMove && colDistanceAbs == 0)
      }
    }
    pieceType match {
      case Pawn => pawnCanMove()
      case _    => canAttack(start, end)
    }
  }

  def allPossibleValidMove(start: Position, moveValidator: MoveValidator): Set[ValidMove] =
    pieceType match {
      case Knight =>
        (Set() + Position(start.col + 1, start.row + 2) + Position(start.col - 1, start.row + 2) +
        Position(start.col + 1, start.row - 2) + Position(start.col - 1, start.row - 2) +
        Position(start.col + 2, start.row + 1) + Position(start.col + 2, start.row - 1) +
        Position(start.col - 2, start.row + 1) + Position(start.col - 2, start.row - 1))
          .flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
      case Pawn =>
        color match {
          case White =>
            (Set() + Position(start.col, start.row + 1) + Position(start.col, start.row + 2) +
            Position(start.col + 1, start.row + 1) + Position(start.col - 1, start.row + 1))
              .flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
          case Black =>
            (Set() + Position(start.col, start.row - 1) + Position(start.col, start.row - 2) +
            Position(start.col + 1, start.row - 1) + Position(start.col - 1, start.row - 1))
              .flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
        }
      case King   => start.adjacentPositions.flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
      case Rook   => allRookValidMoves(start, moveValidator)
      case Bishop => allBishopValidMoves(start, moveValidator, Set())
      case Queen  => allRookValidMoves(start, moveValidator) ++ allBishopValidMoves(start, moveValidator, Set())
    }

  private def allRookValidMoves(start: Position, moveValidator: MoveValidator): Set[ValidMove] = {
    val colRightEndPoint              = Position(Board.width, start.row)
    val colLeftEndPoint               = Position(1, start.row)
    val rowUpEndPoint                 = Position(start.col, Board.height)
    val rowDownEndPoint               = Position(start.col, 1)
    val allPossiblePos: Set[Position] = Set()
    start.generatePosBetweenCol(colRightEndPoint, allPossiblePos) + colRightEndPoint ++
    start.generatePosBetweenCol(colLeftEndPoint, allPossiblePos) + colLeftEndPoint ++
    start.generatePosBetweenRow(rowUpEndPoint, allPossiblePos) + rowUpEndPoint ++
    start.generatePosBetweenRow(rowDownEndPoint, allPossiblePos) + rowDownEndPoint
    allPossiblePos.flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
  }

  private def allBishopValidMoves(start: Position,
                                  moveValidator: MoveValidator,
                                  positions: Set[Position]): Set[ValidMove] = {
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
      .flatMap(pos => moveValidator.validateMove(start, pos, color).toOption)
  }

}
