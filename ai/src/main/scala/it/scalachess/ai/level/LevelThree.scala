package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.pieces.{ Bishop, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.Validation

case class LevelThree() extends Level {

  private val levelTwo = LevelTwo()

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] = ???

  def piecePositionValue(pieceType: PieceType) = pieceType match {
    case Pawn   => ???
    case Knight => ???
    case Bishop => ???
    case Rook   => ???
    case Queen  => ???
  }

}
