package it.scalachess.ai.level

import it.scalachess.ai.movesearch.{MinimaxNode, MinimaxWithQuiescence}
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The level two A.I.'s strategy is trying to play the move which capture the most important piece,
 * but before proceeding with a capture, it will consider all the possible consequences derived
 * by the opponent's next moves.
 */
class LevelTwo() extends LevelOne with MinimaxWithQuiescence {

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(MinimaxNode(board, history), aiPlayer, evaluateBoardByPieces))
  }

}
