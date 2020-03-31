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

case class ParsedSimpleMove(
    endPos: Position,
    pieceType: PieceType,
    capture: (Option[PieceType], Option[Char]),
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
  private def isCaptureEqual(capture: (Option[PieceType], Option[Char]), move: ParsedSimpleMove): Boolean =
    (capture._1 == move.capture._1 && capture._2 == move.capture._2) ||
    (capture._1.isEmpty && capture._2.isEmpty && move.capture._1.isEmpty && move.capture._2.isEmpty)
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
