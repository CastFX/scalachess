package it.scalachess.core.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * Definition of a ParsedMove from Algebraic Notation
 */
sealed trait ParsedMove

case class Move(
    endPos: Position,
    pieceType: PieceType,
    capture: (Option[PieceType], Option[String]),
    check: Boolean,
    checkmate: Boolean,
    startingCol: Option[Char],
    startingRow: Option[Int],
    Promotion: Option[PieceType]
) extends ParsedMove

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
) extends ParsedMove
