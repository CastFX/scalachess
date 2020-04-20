package it.scalachess.core.parser

import it.scalachess.core.logic.moves.{ KingSide, QueenSide }
import it.scalachess.core.parser.Parser.Parser
import it.scalachess.core.pieces._
import scalaz.{ Success, Validation }

/**
 * Parser that parse an Algebraic Move into the string that represents it.
 */
case class GameSaverParser() extends Parser[AlgebraicMove, String] with PGNFormatter[String] {
  val nothing: String = ""
  override def parse(t: AlgebraicMove): Validation[String, String] =
    t match {
      case AlgebraicCastling(QueenSide, check, checkmate) => Success(s"0-0-0${checks(check, checkmate)}")
      case AlgebraicCastling(KingSide, check, checkmate)  => Success(s"0-0${checks(check, checkmate)}")
      case AlgebraicSimpleMove(endPos, pieceType, capture, check, checkmate, col, row, promote) =>
        Success(
          s"""${getPiece(pieceType)}${col
            .getOrElse(nothing)}${row.getOrElse(nothing)}${getCapture(capture)}${endPos.toString}${promote.fold(
            nothing)(pieceType => s"=${getPiece(pieceType)}")}${checks(check, checkmate)}"""
        )
    }

  private def getPiece(piece: PieceType): String =
    piece match {
      case King   => "K"
      case Queen  => "Q"
      case Bishop => "B"
      case Rook   => "R"
      case Knight => "N"
      case _      => nothing
    }

  private def getCapture(capture: Boolean): String =
    if (capture) "x" else nothing

  private def checks(check: Boolean, checkmate: Boolean): String =
    if (checkmate) "#" else if (check) "+" else nothing
}

/**
 * A GameSaverParser that uses the NonAmbiguous mixin to add the
 * possibility to convert a List of FullMoves into a string representing the game.
 */
object NonAmbiguousGameSaver extends GameSaverParser with NonAmbiguous
