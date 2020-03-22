package it.scalachess.core

import it.scalachess.core.board.{ Board }
import it.scalachess.core.colors.{ Color, White }
import it.scalachess.core.logic.{ CheckValidator, MoveValidator }
import scalaz.{ Failure, Success, Validation }

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

  def moveAttempt(move: String): Validation[String, ChessGame] =
    moveValidator.computeMoveErrors(move, player) match {
      case Right(move) =>
        if (checkValidator.controlCheckOnTurnEnd(move, player))
          Failure("This move makes king under check!")
        else {
          Success(ChessGame(board(move), player.other, turn + 1))
        }
      case Left(errorMSg) => Failure(errorMSg)
    }

  def moveAftermath(): Either[String, ChessGame] =
    /*if (checkValidator.controlCheckMate(player))
      Left("Game End")
    else {*/
    if (checkValidator.controlCheck(player))
      Left("The king is under check")
    else {
      Right(ChessGame(board, player, turn))
    }
  //}

}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0)
}
