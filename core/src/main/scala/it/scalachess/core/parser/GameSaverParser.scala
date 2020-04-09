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
      case _: ValidSimpleMove =>
        val validMove: ValidSimpleMove = t.validMove.asInstanceOf[ValidSimpleMove]
        Success(
          s"${getPiece(validMove.pieceType)}${validMove.from.toString}${getCapture(validMove.capture)}${validMove.to.toString}$check")
      case _: ValidPromotion =>
        val validMove: ValidPromotion = t.validMove.asInstanceOf[ValidPromotion]
        val promote                   = s"=${getPiece(validMove.promotesTo.pieceType)}"
        Success(
          s"${getPiece(validMove.pieceType)}${validMove.from.toString}${getCapture(validMove.capture)}${validMove.to.toString}$promote$check")
      case _: ValidEnPassant =>
        val validMove: ValidEnPassant = t.validMove.asInstanceOf[ValidEnPassant]
        Success(s"${getPiece(validMove.pieceType)}${validMove.from.toString}x${validMove.to.toString}$check")
    }
  }
  def parseAndConvert(seq: Seq[FullMove]): String =
    (for (group <- parseAll(seq).flatMap(_.toOption).grouped(2))
      yield group.mkString(nothing, " ", "\n")).zipWithIndex.map(elem => s"${elem._2 + 1}.${elem._1}").mkString

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
