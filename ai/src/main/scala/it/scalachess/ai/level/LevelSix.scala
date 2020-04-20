package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

/**
 * The level six A.I.'s uses a minimax depth = 3, with quiescence search.
 */
class LevelSix() extends LevelFour {

  override protected val quiescenceSearchActive = true
  override protected val minimaxDepth: Int = 3

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(board, history, aiPlayer, evaluatePiecesAndTheirPosInBoard))
  }

}