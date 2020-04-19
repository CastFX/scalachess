package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

class LevelFive() extends LevelFour {

  override protected val quiescenceSearchActive = false
  override protected val minimaxDepth: Int = 3

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    verifyGameIsPlayable(board, aiPlayer, history)
    moveWithMaxEval(minimax(board, history, aiPlayer, minimaxDepth, evaluatePiecesAndTheirPosInBoard))
  }

}