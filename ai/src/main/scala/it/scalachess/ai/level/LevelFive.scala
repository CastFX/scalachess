package it.scalachess.ai.level

import it.scalachess.ai.movesearch.MinimaxNode
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The level five A.I.'s uses a minimax depth = 3, without quiescence search.
 */
class LevelFive() extends LevelFour {

  override protected val quiescenceSearchActive = false
  override protected val minimaxDepth: Int = 3

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(MinimaxNode(board, history), aiPlayer, evaluatePiecesAndTheirPosInBoard))
  }

}