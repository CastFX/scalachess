package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.pieces.{ Bishop, Knight, Pawn, PieceType, Queen, Rook }

case class LevelThree() extends Level {

  private val levelTwo = LevelTwo()

  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove = ???

  def piecePositionValue(pieceType: PieceType) = pieceType match {
    case Pawn   => ???
    case Knight => ???
    case Bishop => ???
    case Rook   => ???
    case Queen  => ???
  }

}
