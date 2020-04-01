package it.scalachess.core

import it.scalachess.core.board.Board
import it.scalachess.core.logic.{ IsKingInCheck, IsKingInCheckmate, MoveValidator }
import it.scalachess.core.parser.Parser.AlgebraicParser
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
    moveHistory: List[String]
) {

  private val moveValidator = MoveValidator(board)

  def apply(move: String): Validation[String, ChessGame] = {
    val parser: AlgebraicParser = AlgebraicParser()
    parser.parse(move) match {
      case None => Failure("Not algebraic format, insert another move ")
      case Some(parsedMove) =>
        moveValidator.validateParsedMove(parsedMove, player) match {
          case Success(boardMove) =>
            val nextBoard = board(boardMove)
            if (IsKingInCheckmate(player.other, nextBoard))
              Success(
                ChessGame(nextBoard,
                          player.other,
                          turn + 1,
                          Win(player),
                          isKingInCheck = true,
                          moveHistory ::: List(move)))
            else {
              Success(
                ChessGame(nextBoard,
                          player.other,
                          turn + 1,
                          Ongoing,
                          IsKingInCheck(player.other, nextBoard),
                          moveHistory ::: List(move)))
            }
          case Failure(errorMsg) => Failure(errorMsg)
        }
    }
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
