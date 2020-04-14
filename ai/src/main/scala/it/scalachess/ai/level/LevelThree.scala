package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.Validation

case class LevelThree() extends Level {

  private val levelTwo = LevelTwo()

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] = ???

  private def piecePositionValue(pieceType: PieceType): Double =
    pieceType match {
      case Pawn   => pawnValue
      case Knight => knightValue
      case Bishop => bishopValue
      case Rook   => rookValue
      case Queen  => queenValue
      case King   => kingValue
      case _ =>
        assert(assertion = false, s"The AI doesn't know the value of this piece: $pieceType")
        0
    }
  private val pawnValue   = 10
  private val knightValue = 30
  private val bishopValue = 35
  private val rookValue   = 50
  private val queenValue  = 100
  private val kingValue   = 1000

}
