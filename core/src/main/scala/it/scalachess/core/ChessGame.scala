package it.scalachess.core

import it.scalachess.core.board.Board
import it.scalachess.core.colors.{ Color, White }
import it.scalachess.core.gamestatus.{ GameStatus, Ongoing }
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
    turn: Int,
    gameStatus: GameStatus,
    isKingInCheck: Boolean
) {

  private val moveValidator = MoveValidator(board)

  def apply(move: String): Validation[String, ChessGame] =
    moveValidator.validate(move, player) match {
      case Success(move) =>
        val checkValidator = CheckValidator(moveValidator, board.apply(move))
        checkValidator.validateMoveFromAllyKingCheck(player) match {
          case Success(isAllyKingInCheck) =>
            if (isAllyKingInCheck)
              Failure("This move makes king under check!")
            else {
              // TODO checkmate
              checkValidator.isOppositeKingInCheck(player) match {
                case Success(result) =>
                  Success(ChessGame(board(move), player.other, turn + 1, Ongoing, result))
                case Failure(errorMsg) => Failure(errorMsg)
              }
            }
          case Failure(errorMsg) => Failure(errorMsg)
        }
      case Failure(errorMSg) => Failure(errorMSg)
    }

}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0, Ongoing, false)
}
