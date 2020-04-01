package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * Definition of a ParsedMove from Algebraic Notation
 */
sealed trait ParsedMove {

  /**
   * Check if the ParsedMove is equal to another ParsedMove.
   * @param move the ParsedMove to check against
   * @return true if the moves are equals, false otherwise
   */
  def isEqualTo(move: ParsedMove): Boolean
}

case class Capture(attackingPieceType: Option[PieceType], column: Option[Char])

case class ParsedSimpleMove(
    endPos: Position,
    pieceType: PieceType,
    capture: Capture,
    check: Boolean,
    checkmate: Boolean,
    startingCol: Option[Char],
    startingRow: Option[Int],
    promotion: Option[PieceType]
) extends ParsedMove {
  override def isEqualTo(move: ParsedMove): Boolean =
    move match {
      case move: ParsedSimpleMove =>
        endPos == move.endPos &&
        pieceType == move.pieceType &&
        isCaptureEqual(capture, move) &&
        check == move.check &&
        checkmate == move.checkmate &&
        isStartingPointEqual(startingCol, startingRow, move) &&
        promotion == move.promotion
      case _ => false
    }
  private def isCaptureEqual(capture: Capture, move: ParsedSimpleMove): Boolean =
    (capture.attackingPieceType == move.capture.attackingPieceType && capture.column == move.capture.column) ||
    (capture.attackingPieceType == move.capture.attackingPieceType && capture.column.isEmpty) ||
    (capture.attackingPieceType.isEmpty && capture.column.isEmpty && move.capture.attackingPieceType.isEmpty && move.capture.column.isEmpty)
  private def isStartingPointEqual(startingCol: Option[Char],
                                   startingRow: Option[Int],
                                   move: ParsedSimpleMove): Boolean =
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
case class Castling(
    castlingType: CastlingType,
    check: Boolean,
    checkmate: Boolean
) extends ParsedMove {
  override def isEqualTo(move: ParsedMove): Boolean =
    move match {
      case move: Castling =>
        castlingType == move.castlingType &&
        check == move.check &&
        checkmate == move.checkmate
      case _ => false
    }
}
