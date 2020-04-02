package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * Definition of a ParsedMove from Algebraic Notation
 */
sealed trait AlgebraicMove {

  /**
   * Check if the ParsedMove is equal to another ParsedMove.
   * @param move the ParsedMove to check against
   * @return true if the moves are equals, false otherwise
   */
  def isEqualTo(move: AlgebraicMove): Boolean
}

case class Capture(attackingPieceType: Option[PieceType], column: Option[Char])

case class AlgebraicSimpleMove(
    endPos: Position,
    pieceType: PieceType,
    capture: Capture,
    check: Boolean,
    checkmate: Boolean,
    startingCol: Option[Char],
    startingRow: Option[Int],
    promotion: Option[PieceType]
) extends AlgebraicMove {
  override def isEqualTo(move: AlgebraicMove): Boolean =
    move match {
      case move: AlgebraicSimpleMove =>
        endPos == move.endPos &&
        pieceType == move.pieceType &&
        isCaptureEqual(capture, move) &&
        ((check == move.check && checkmate == move.checkmate) || (checkmate && checkmate == move.checkmate)) &&
        isStartingPointEqual(startingCol, startingRow, move) &&
        promotion == move.promotion
      case _ => false
    }
  private def isCaptureEqual(capture: Capture, move: AlgebraicSimpleMove): Boolean =
    (capture.attackingPieceType == move.capture.attackingPieceType && capture.column == move.capture.column) ||
    (capture.attackingPieceType == move.capture.attackingPieceType && capture.column.isEmpty) ||
    (capture.attackingPieceType.isEmpty && capture.column.isEmpty && move.capture.attackingPieceType.isEmpty && move.capture.column.isEmpty)
  private def isStartingPointEqual(startingCol: Option[Char],
                                   startingRow: Option[Int],
                                   move: AlgebraicSimpleMove): Boolean =
    (startingCol.isDefined && startingCol == move.startingCol || startingCol.isEmpty) &&
    (startingRow.isDefined && startingRow == move.startingRow || startingRow.isEmpty)
}

/**
 * Definition of the possible types of castling
 */
sealed trait CastlingType

/**
 * Definition of a King Side castling, also called short castling
 */
case object KingSide extends CastlingType

/**
 * Definition of a Queen Side castling, also called long castling
 */
case object QueenSide extends CastlingType

/**
 * Definition of a castling
 * @param castlingType the type of castling
 */
case class AlgebraicCastling(
    castlingType: CastlingType,
    check: Boolean,
    checkmate: Boolean
) extends AlgebraicMove {
  override def isEqualTo(move: AlgebraicMove): Boolean =
    move match {
      case move: AlgebraicCastling =>
        castlingType == move.castlingType &&
        ((check == move.check && checkmate == move.checkmate) || (checkmate && checkmate == move.checkmate))
      case _ => false
    }
}
