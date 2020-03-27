package it.scalachess.core.logic

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.{Piece, PieceType}

// TODO ----- CODICE DELLO SPRINT 2

sealed trait ParsedMove
case class Move(endPos: Position,
                pieceType: PieceType,
                capture: Boolean,
                check: Boolean,
                checkmate: Boolean,
                startingCol: Option[Char],
                startingRow: Option[Int],
                Promotion: Option[PieceType])
    extends ParsedMove
case class Castling(castlingType: CastlingType)          extends ParsedMove
case class EnPassant(capture: Position)                  extends ParsedMove
case class Promotion(to: Position, pieceType: PieceType) extends ParsedMove

sealed trait CastlingType
case object KingType  extends CastlingType
case object QueenType extends CastlingType
