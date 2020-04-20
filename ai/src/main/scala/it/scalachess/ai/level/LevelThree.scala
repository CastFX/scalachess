package it.scalachess.ai.level

import it.scalachess.ai.movesearch.MinimaxNode
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The level two A.I.'s before play a move, it will consider all the possible consequences
 * derived by the opponent's next moves, because it uses a minimax depth = 2.
 */
class LevelThree() extends LevelTwo {

  override protected val minimaxDepth: Int = 2

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(MinimaxNode(board, history), aiPlayer, evaluateBoardByPieces))
  }

}