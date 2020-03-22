package it.scalachess.core

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.{ Color, White }
import it.scalachess.core.logic.{ CheckValidator, MoveValidator }

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

  private val moveValidator  = MoveValidator(board)
  private val checkValidator = CheckValidator(board, moveValidator)

  def moveAttempt(move: String): ChessGame = // Validation[String, ChessGame] =
    moveValidator.computeMoveErrors(move, player) match {
      case Right(move) =>
        if (checkValidator.checkOnTheEndTurn(move, player))
          ChessGame(board, player, turn)
        // Failure("This move makes king under check!")
        else {
          board.applyMove(move)
          ChessGame(board, player.other, turn + 1)
          // Success(ChessGame(board, player.other, turn + 1))
        }
      // case Left(string) => Failure(string)
    }

  def moveAftermath(): Either[String, ChessGame] =
    if (checkValidator.checkMate(player))
      Left("Game End")
    else {
      if (checkValidator.check(player))
        Left("The player is under check")
      else {
        Right(ChessGame(board, player, turn))
      }
    }

}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0)
}
