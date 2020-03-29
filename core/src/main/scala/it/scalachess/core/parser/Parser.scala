package it.scalachess.core.parser
import it.scalachess.core.board.Position
import it.scalachess.core.moves.{ Castling, KingSide, Move, ParsedMove, QueenSide }
import it.scalachess.core.parser.Parser.AlgebraicParser
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }

import scala.util.matching.Regex

object Parser {
  abstract class Parser[T] {
    def parse(t: T): Option[ParsedMove]
    def parseAll(seq: Seq[T]): Seq[Option[ParsedMove]] = seq map parse
  }

  case class AlgebraicParser() extends Parser[String] {
    private val promotablePieces = "[N,B,R,Q]"
    private val pieces           = s"$promotablePieces|K"
    private val cols             = "[a-h]"
    private val rows             = "[1-8]"
    private val capture          = s"(?:$pieces|$cols)x"
    private val check            = "\\+"
    private val checkmate        = "#"
    private val castleRegex      = s"0-0(-0)?($check)?($checkmate)?".r
    private val movePattern: Regex =
      s"($pieces)?($cols)?($rows)?($capture)?($cols$rows)(=$promotablePieces)?($check)?($checkmate)?".r

    override def parse(t: String): Option[ParsedMove] =
      t match {
        case movePattern(pieces, cols, rows, captured, position, promotable, checked, checkmated) =>
          val endPos: Position                             = Position.ofNotation(position).get
          val capture: (Option[PieceType], Option[String]) = isCaptured(captured)
          val check: Boolean                               = isChecked(checked)
          val checkmate: Boolean                           = isCheckmated(checkmated)
          val col: Option[Char]                            = startingCol(cols)
          val row: Option[Int]                             = startingRow(rows)
          val promotion: Option[PieceType]                 = promotionOf(promotable)

          val pieceType: PieceType = capture match {
            case (Some(piece), _) => Some(piece).get
            case _                => pieceOfType(pieces)
          }
          if (promotion.isDefined && pieceType != Pawn)
            None
          else
            Some(Move(endPos, pieceType, capture, check, checkmate, col, row, promotion))
        case castleRegex(queenSide, check, checkmate) =>
          Some(Castling(if (queenSide == null) KingSide else QueenSide, isChecked(check), isCheckmated(checkmate)))
        case _ => None
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
  private def isCaptured(captured: String): (Option[PieceType], Option[String]) =
    if (captured == null) (None, None)
    else {
      captured.charAt(0) match {
        case 'K' | 'Q' | 'N' | 'R' | 'B' => (Some(pieceOfType(captured.charAt(0).toString)), None)
        case _                           => (Some(Pawn), Some(captured.charAt(0).toString))
      }
    }
  private def isChecked(checked: String): Boolean       = if (checked == null) false else true
  private def isCheckmated(checkmated: String): Boolean = if (checkmated == null) false else true
  private def startingCol(cols: String): Option[Char]   = if (cols == null) None else Some(cols.charAt(0))
  private def startingRow(rows: String): Option[Int]    = if (rows == null) None else Some(rows.toInt)
  private def promotionOf(promoted: String): Option[PieceType] =
    promoted match {
      case "=K" => Some(King)
      case "=Q" => Some(Queen)
      case "=N" => Some(Knight)
      case "=B" => Some(Bishop)
      case "=R" => Some(Rook)
      case _    => None
    }

}

object Main {
  def main(args: Array[String]): Unit = {
    val test: Seq[String] =
      Seq("Ra3", "e4", "Bb5", "0-0", "0-0-0", "Rae8", "Ra6+", "a4#", "Nb6d7", "xe4", "exe4", "Qxe4", "a3=Q", "Qa3=Q")
    val parser: AlgebraicParser = AlgebraicParser()
    for (elem <- parser.parseAll(test)) {
      println(elem)
    }
  }
}
