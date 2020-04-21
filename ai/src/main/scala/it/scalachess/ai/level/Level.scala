package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

/**
 * The interface which manages the levels implementation.
 */
trait Level extends ((Board, Seq[FullMove], Color) => FullMove) {

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove

  /**
   * Checks if the board represents a playable game (means the board is not in checkmate situation).
   * If the board doesn't respect this requirement: throws IllegalArgumentException exception
   * specifying the player in checkmate.
   * @param board the board to be checked
   * @param history the history of the moves played during this game
   * @param aiPlayer the AI player color
   */
  protected def verifyGameIsPlayable(board: Board, history: Seq[FullMove], aiPlayer: Color): Unit = {
    require(new MoveGenerator(board, aiPlayer, history).allMoves().nonEmpty, Level.aiIsInCheckmateErrorMsg)
    require(new MoveGenerator(board, aiPlayer.other, history).allMoves().nonEmpty, Level.opponentIsInCheckmateErrorMsg)
  }

}

object Level {
  val aiIsInCheckmateErrorMsg = "The A.I.'s possible moves seq is empty (probably is it in checkmate?): wrong usage of the AI"
  val opponentIsInCheckmateErrorMsg = "The opponent player's possible moves seq is empty (probably is he/she in checkmate?): wrong usage of AI"
}
