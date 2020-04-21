package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.util.Random

/**
 * The level zero AI plays a random move.
 */
class LevelZero() extends Level {

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    randomMove(new MoveGenerator(board, aiPlayer, history).allMoves())
  }

  /**
   * Returns a random move from the input seq.
   * @param aiMoves moves seq playable by the A.I.
   * @return a random FullMove
   */
  protected def randomMove(aiMoves: Seq[FullMove]): FullMove = {
    aiMoves(Random.nextInt(aiMoves.size))
  }

}
