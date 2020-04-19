package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

/**
 * The interface which manages level implementation.
 */
trait Level extends ((Board, Color, Seq[FullMove]) => FullMove) {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove

  /**
   * Checks if the board represents a playable game (the board is not in checkmate situation).
   * If the board doesn't have this requirement: throws IllegalArgumentException exception specifying
   * the player in checkmate.
   * @param board the board to be checked
   * @param aiPlayer the AI player color
   * @param history the history of the moves played during this game
   */
  protected def verifyGameIsPlayable(board: Board, aiPlayer: Color, history: Seq[FullMove]): Unit = {
    require(new MoveGenerator(board, aiPlayer, history).allMoves().nonEmpty, Level.aiIsInCheckmateErrorMsg)
    require(new MoveGenerator(board, aiPlayer.other, history).allMoves().nonEmpty, Level.opponentIsInCheckmateErrorMsg)
  }

}

object Level {
  val aiIsInCheckmateErrorMsg = "The AI's possible moves list is empty (probably is it in checkmate?): wrong usage of the AI"
  val opponentIsInCheckmateErrorMsg = "The opponent player's possible moves list is empty (probably is it in checkmate?): wrong usage of AI"
}
