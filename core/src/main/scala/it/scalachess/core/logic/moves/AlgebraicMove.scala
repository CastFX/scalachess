package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * Definition of move created from Algebraic Notation
 */
sealed trait AlgebraicMove

/**
 * Definition of a simple move created from Algebraic Notation. It incorporates en passant and promotion.
 * @param endPos the ending position of the piece
 * @param pieceType the type of the piece
 * @param capture true if the move results in a capture, false otherwise
 * @param check true if the move results in a check, false otherwise
 * @param checkmate true if the move results in a checkmate, false otherwise
 * @param startingCol the starting column of the piece if defined
 * @param startingRow the ending column of the piece if defined
 * @param promotion the piece to be promoted to if defined
 */
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
 * Definition of a castling created from Algebraic Notation
 * @param castlingType the type of the castling: KingSide or QueenSide
 * @param check true if the move results in a check, false otherwise
 * @param checkmate true if the move results in a checkmate, false otherwise
 */
case class AlgebraicCastling(
    castlingType: CastlingType,
    check: Boolean,
    checkmate: Boolean
) extends AlgebraicMove
