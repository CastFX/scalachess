package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.util.Random

/**
 * The level zero AI plays random moves.
 */
final case class LevelZero() extends Level {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    val moves = new MoveGenerator(board: Board, aiPlayer: Color, history: Seq[FullMove]).allMoves()
    assert(moves.nonEmpty, aiPlayerInCheckmateFailMsg)
    moves(Random.nextInt(moves.size))
  }

}
