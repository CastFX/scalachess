package it.scalachess.core

import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.ParsedMove
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
    isKingInCheck: Boolean,
    moveHistory: List[ParsedMove]
) {

  private val moveValidator  = MoveValidator(board)
  private val checkValidator = CheckValidator()

  def apply(move: ParsedMove): Validation[String, ChessGame] =
    moveValidator.validateParsedMove(move, player) match {
      case Success(boardMove) =>
        board(boardMove) match {
          case Success(nextBoard) =>
            if (checkValidator.isKingInCheckmate(player, MoveValidator(nextBoard)))
              Success(
                ChessGame(nextBoard,
                          player.other,
                          turn + 1,
                          Win(player),
                          isKingInCheck = true,
                          moveHistory ::: List(move)))
            else {
              checkValidator.isKingInCheck(player, nextBoard) match {
                case Success(result) =>
                  Success(ChessGame(nextBoard, player.other, turn + 1, Ongoing, result, moveHistory ::: List(move)))
                case Failure(errorMsg) =>
                  Failure(errorMsg)
              }
            }
          case Failure(errorMsg) => Failure(errorMsg)
        }
      case Failure(errorMsg) => Failure(errorMsg)
    }
}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0, Ongoing, isKingInCheck = false, List())
}
