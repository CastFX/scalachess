package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.util.Random

/**
 * The level zero AI plays random moves.
 */
class LevelZero() extends Level {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    opponentNotInCheckmate(board, aiPlayer, history)
    randomMove(new MoveGenerator(board, aiPlayer, history).allMoves())
  }

  /**
   * Returns a random move from the input list.
   * @param moves the eligible moves list
   * @return a random FullMove
   */
  protected def randomMove(moves: List[FullMove]): FullMove = {
    require(moves.nonEmpty, Level.aiIsInCheckmateErrorMsg)
    moves(Random.nextInt(moves.size))
  }

}
