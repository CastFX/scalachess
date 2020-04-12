package it.scalachess.core.parser

import it.scalachess.core.board.Position
import it.scalachess.core.logic.moves.{
  FullMove,
  KingSide,
  QueenSide,
  ValidCastling,
  ValidEnPassant,
  ValidPromotion,
  ValidSimpleMove
}
import it.scalachess.core.parser.Parser.Parser
import it.scalachess.core.pieces.{ Bishop, King, Knight, PieceType, Queen, Rook }
import scalaz.{ Success, Validation }

/**
 * Can be used to parse the move of a game into a string that sum up the game.
 */
object GameSaverParser extends Parser[FullMove, String] {
  val nothing: String = ""
  override def parse(t: FullMove): Validation[String, String] = {
    val check = if (t.resultsInCheckmate) "#" else if (t.resultsInCheck) "+" else nothing
    t.validMove match {
      case ValidCastling(_, _, _, _, _, QueenSide) => Success(s"0-0-0$check")
      case ValidCastling(_, _, _, _, _, KingSide)  => Success(s"0-0$check")
      case simple: ValidSimpleMove =>
        Success(
          s"${getPiece(simple.pieceType)}${simple.from.toString}${getCapture(simple.capture)}${simple.to.toString}$check")
      case promotion: ValidPromotion =>
        val promote = s"=${getPiece(promotion.promotesTo.pieceType)}"
        Success(
          s"${getPiece(promotion.pieceType)}${promotion.from.toString}${getCapture(promotion.capture)}${promotion.to.toString}$promote$check")
      case enpassant: ValidEnPassant =>
        Success(s"${getPiece(enpassant.pieceType)}${enpassant.from.toString}x${enpassant.to.toString}$check")
    }
  }

  /**
   * Parse the input into a string representing the game
   * @param seq the input to be parsed
   * @return a representation of the game
   */
  def parseAndConvert(seq: Seq[FullMove]): String =
    (for (group <- parseAll(seq).flatMap(_.toOption).grouped(2))
      yield group.mkString(nothing, " ", "\n")).zipWithIndex.map {
      case (moves: String, index: Int) => s"${index + 1}.$moves"
    }.mkString

  private def getPiece(piece: PieceType): String =
    piece match {
      case King   => "K"
      case Queen  => "Q"
      case Bishop => "B"
      case Rook   => "R"
      case Knight => "N"
      case _      => nothing
    }

  private def getCapture(capture: Option[Position]): String =
    if (capture.isDefined) "x" else nothing
}
