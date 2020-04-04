package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * Definition of a ParsedMove from Algebraic Notation
 */
sealed trait AlgebraicMove

case class AlgebraicSimpleMove(
    endPos: Position,
    pieceType: PieceType,
    capture: Boolean,
    check: Boolean,
    checkmate: Boolean,
    startingCol: Option[Char],
    startingRow: Option[Int],
    promotion: Option[PieceType]
) extends AlgebraicMove

/**
 * Definition of a castling
 * @param castlingType the type of castling
 */
case class AlgebraicCastling(
    castlingType: CastlingType,
    check: Boolean,
    checkmate: Boolean
) extends AlgebraicMove
