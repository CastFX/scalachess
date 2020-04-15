package it.scalachess.core

import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

/**
 * The status of a chess game
 */
sealed trait GameStatus

/**
 * The ending result of a chess game
 */
sealed trait Result extends GameStatus

/**
 * The status of a game that is on going
 */
case object Ongoing extends GameStatus

/**
 * The status of a game that ended with a draw
 */
case object Draw extends Result

/**
 * The status of a game that ended with a win
 * @param player the player that won the game
 */
case class Win(player: Color) extends Result

/**
 * The status of a game that ended with a win by forfeit
 * @param player the other player that won the game by forfeit
 */
case class WinByForfeit(player: Color) extends Result

case object GameStatus {
  def currentStatus(lastMove: FullMove, chessGame: ChessGame): GameStatus =
    if (lastMove.resultsInCheckmate) Win(lastMove.validMove.color)
    else if (stalemate(lastMove, chessGame.moveHistory)) Draw
    else Ongoing

  private def stalemate(lastMove: FullMove, history: Seq[FullMove]): Boolean = {
    val moveGen = new MoveGenerator(lastMove.boardAfter, lastMove.validMove.color.other, history)
    !lastMove.resultsInCheck && moveGen.allMoves().isEmpty
  }
}
