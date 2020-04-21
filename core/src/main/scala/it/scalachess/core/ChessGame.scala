package it.scalachess.core

import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.{ GameStatus, MoveValidator, Ongoing, Result }
import it.scalachess.core.parser.Parser.AlgebraicParser
import scalaz.{ Failure, Success, Validation }

/**
 * Functional Representation of a game of Chess
 * @param board The board which contains all the chess pieces
 * @param player The Player (White/Black) who is moving
 * @param turn The turn number of this game
 * @param gameStatus the status of the game
 * @param moveHistory the history of the moves of this chess game
 */
final case class ChessGame(
    board: Board,
    player: Color,
    turn: Int,
    gameStatus: GameStatus,
    moveHistory: Seq[FullMove]
) {
  lazy val isKingInCheck: Boolean = moveHistory.lastOption.fold(false)(_.resultsInCheck)

  def apply(move: String): Validation[String, ChessGame] = gameStatus match {
    case Ongoing =>
      AlgebraicParser.parse(move) match {
        case Failure(error) => Failure(error)
        case Success(algebraicMove) =>
          MoveValidator(board, player, moveHistory)(algebraicMove) match {
            case Success(fullMove)      => Success(apply(fullMove))
            case error: Failure[String] => error
          }
      }
    case _ => Failure("The game is not ongoing")
  }

  def apply(fullMove: FullMove): ChessGame = {
    val nextBoard = board(fullMove.validMove.boardChanges)
    val result    = GameStatus.currentStatus(fullMove, this)
    ChessGame(nextBoard, player.other, turn + 1, result, moveHistory :+ fullMove)
  }

  def end(withResult: Result): ChessGame = ChessGame(board, player, turn, withResult, moveHistory)
}

object ChessGame {

  /**
   * Initialize a standard game of chess with all the piece in the starting positions
   * @return An initialized ChessGame instance
   */
  def standard(): ChessGame =
    ChessGame(Board.defaultBoard(), White, 0, Ongoing, Seq())
}
