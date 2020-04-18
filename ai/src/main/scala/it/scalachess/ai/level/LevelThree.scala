package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.{MoveGenerator, linearMovement}
import it.scalachess.core.pieces.{Bishop, King, Knight, Pawn, PieceType, Queen, Rook}

import scala.util.Random

/**
 * The level Three AI plays TODO
 */
class LevelThree() extends LevelTwo {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    val moves = new MoveGenerator(board: Board, aiPlayer: Color, history: Seq[FullMove]).allMoves()
    moves(Random.nextInt(moves.size))
  }

  /**
   * Evaluates a piece relying on his type
   * @param pieceType the type of the piece to evaluate
   * @return the evalutation of that piece type
   */
  private def pieceValueByHisPosition(pieceType: PieceType): Double =
    pieceType match {
      /*case Pawn   => pawnValue
      case Knight => knightValue
      case Bishop => bishopValue
      case Rook   => rookValue
      case Queen  => queenValue
      case King   => kingValue
       */
      case _ =>
        assert(assertion = false, s"The AI doesn't know the value of this piece: $pieceType")
        0
    }
/*
  private val whitePawnPositionValue = Board.allPosition().map(pos => pos match {
    case Position(_, 1) || Position(_, 8) => (pos, 0)
    case Position(_, 7) => (pos, 5)
    case Position(_, 6) => pos.col match {
      case 1 || 2 || 7 || 8 => (pos, 1)
      case 3 || 6 => (pos, 2)
      case 4 || 5 => (pos, 3)
    }
    case pos.row == 5 => pos.col match {
      case 1 || 2 || 7 || 8 => (pos, 0.5)
      case 3 || 6 => (pos, 1)
      case 4 || 5 => (pos, 2.5)
    }
    case pos.row == 4 => pos.col match {
      case 1 || 2 || 3 || 6 || 7 || 8 => (pos, 0)
      case 4 || 5 => (pos, 2)
    }
    case pos.row == 3 => pos.col match {
      case 1 || 8 => (pos, 0.5)
      case 2 || 7 => (pos, -0.5)
      case 3 || 6 => (pos, -1)
      case 4 || 5 => (pos, 0)
    }
    case pos.row == 2 => pos.col match {
      case 1 || 8 => (pos, 0.5)
      case 2 || 7 || 3 || 6 => (pos, 1)
      case 4 || 5 => (pos, -2)
    }
  }
  )
*/

}