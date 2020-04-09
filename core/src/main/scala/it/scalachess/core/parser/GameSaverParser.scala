package it.scalachess.core.parser

import it.scalachess.core.{ Black, White }
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
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.{ Success, Validation }

/**
 * Can be used to parse the move of a game into a string that sum up the game.
 */
object GameSaverParser extends Parser[FullMove, String] {
  override def parse(t: FullMove): Validation[String, String] = {
    val validMove = t.validMove
    val check     = if (t.resultsInCheckmate) "#" else if (t.resultsInCheck) "+" else ""
    validMove match {
      case ValidCastling(_, _, _, _, _, QueenSide) => Success(s"0-0-0$check")
      case ValidCastling(_, _, _, _, _, KingSide)  => Success(s"0-0$check")
      case _: ValidSimpleMove =>
        val piece   = getPiece(validMove.pieceType)
        val capture = if (validMove.capture.isEmpty) "" else "x"
        Success(s"$piece${validMove.from.toString}$capture${validMove.to.toString}$check")
//      case _: ValidPromotion => ???
//      case _: ValidEnPassant => ???
    }
  }
  def parseAndConvert(seq: Seq[FullMove]): String =
    (for (group <- parseAll(seq).flatMap(_.toOption).grouped(2))
      yield group.mkString("", " ", "\n")).zipWithIndex.map(elem => s"${elem._2 + 1}.${elem._1}").mkString

  private def getPiece(piece: PieceType): String =
    piece match {
      case Pawn   => ""
      case King   => "K"
      case Queen  => "Q"
      case Bishop => "B"
      case Rook   => "R"
      case Knight => "N"
    }
}

object Main extends App {
  val v = ValidSimpleMove(Position(1, 2), Position(1, 3), Pawn, White, Some(Position(1, 3)))
  val s = ValidSimpleMove(Position(1, 2), Position(1, 3), Rook, White, Some(Position(1, 3)))
  val c = ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide)
  val q = ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide)
  print(
    GameSaverParser.parseAndConvert(Seq(
      FullMove(v, true, true),
      FullMove(v, false, false),
      FullMove(s, false, false),
      FullMove(v, true, false),
      FullMove(v, false, true),
      FullMove(c, true, false),
      FullMove(q, false, true)
    )))
  //  print(GameSaverParser.parse(FullMove(v, true, false)).toOption.get)
}
