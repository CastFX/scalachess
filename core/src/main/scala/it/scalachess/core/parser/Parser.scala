package it.scalachess.core.parser
import it.scalachess.core.board.Position
import it.scalachess.core.logic.moves.{ AlgebraicCastling, AlgebraicMove, AlgebraicSimpleMove, KingSide, QueenSide }
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

import scala.util.matching.Regex

object Parser {
  abstract class Parser[T] {
    def parse(t: T): Validation[String, AlgebraicMove]
    def parseAll(seq: Seq[T]): Seq[Validation[String, AlgebraicMove]] = seq map parse
  }

  /**
   * Can be used to parse a move that the player writes as input
   */
  object AlgebraicParser extends Parser[String] {
    private val promotablePieces = "[N,B,R,Q]"
    private val pieces           = s"$promotablePieces|K"
    private val cols             = "[a-h]"
    private val rows             = "[1-8]"
    private val capture          = s"x"
    private val check            = "\\+"
    private val checkmate        = "#"
    private val castleRegex      = s"0-0(-0)?($check)?($checkmate)?".r
    private val movePattern: Regex =
      s"($pieces)?($cols)?($rows)?($capture)?($cols$rows)(=$promotablePieces)?($check)?($checkmate)?".r
    private val parsingError = "The move inserted is not in algebraic format, insert another move."

    /**
     * Parse the given player input
     * @param t the input of the player
     * @return an AlgebraicMove if the input can be parsed, None otherwise
     */
    override def parse(t: String): Validation[String, AlgebraicMove] =
      t match {
        case movePattern(pieces, cols, rows, captured, position, promotable, checked, checkmated) =>
          val endPos: Position             = Position.ofNotation(position).get
          val checkmate: Boolean           = isCheckmated(checkmated)
          val check: Boolean               = isChecked(checked)
          val col: Option[Char]            = startingCol(cols)
          val row: Option[Int]             = startingRow(rows)
          val capture: Boolean             = captured != null
          val promotion: Option[PieceType] = promotionOf(promotable)
          val pieceType: PieceType         = pieceOfType(pieces)
          if (capture && (pieces == null && cols == null && rows == null))
            Failure(parsingError)
          else
            Success(AlgebraicSimpleMove(endPos, pieceType, capture, check, checkmate, col, row, promotion))
        case castleRegex(queenSide, check, checkmate) =>
          Success(
            AlgebraicCastling(if (queenSide == null) KingSide else QueenSide,
                              isChecked(check),
                              isCheckmated(checkmate)))
        case _ => Failure(parsingError)
      }
  }

  private def pieceOfType(pieces: String): PieceType =
    pieces match {
      case "K" => King
      case "Q" => Queen
      case "N" => Knight
      case "B" => Bishop
      case "R" => Rook
      case _   => Pawn
    }

  private def isChecked(checked: String): Boolean       = checked != null
  private def isCheckmated(checkmated: String): Boolean = checkmated != null
  private def startingCol(cols: String): Option[Char]   = if (cols == null) None else Some(cols.charAt(0))
  private def startingRow(rows: String): Option[Int]    = if (rows == null) None else Some(rows.toInt)
  private def promotionOf(promoted: String): Option[PieceType] =
    promoted match {
      case "=Q" => Some(Queen)
      case "=N" => Some(Knight)
      case "=B" => Some(Bishop)
      case "=R" => Some(Rook)
      case _    => None
    }

}
