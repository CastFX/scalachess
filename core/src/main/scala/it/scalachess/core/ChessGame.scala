package it.scalachess.core

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.{ Color, White }

/**
 * Functional Representation of a game of Chess
 * @param board The board which contains all the chesspieces
 * @param player The Player (White/Black) who is moving
 * @param turn The turn number of this game
 */
final case class ChessGame(
    board: Board,
    player: Color,
    turn: Int
) {

  def move(from: Position, to: Position): ChessGame =
    ChessGame(board, player.other, turn + 1)
}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0)
}
