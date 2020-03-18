package it.scalachess.core

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.{ Color, White }

case class ChessGame(
    board: Board,
    player: Color,
    turn: Int = 1
) {

  def move(from: Position, to: Position): ChessGame =
    ChessGame(board, player.other, turn + 1)
}

object ChessGame {
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0)
}
