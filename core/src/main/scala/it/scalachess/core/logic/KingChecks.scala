package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators
import it.scalachess.core.logic.moves.generators.MovesGenerator
import it.scalachess.core.pieces.{ King, Piece }

object IsKingInCheck extends Check {
  private val isKingExposedToCheck = false

  /**
   * Checks if the king is in play, and then verifies if it is under check
   * @param exposedPlayer the player who may be exposed to king check
   * @param board in which calculate the check
   * @return true if the king is in check
   */
  override def apply(exposedPlayer: Color, board: Board): Boolean = {
    requireExposedKing(exposedPlayer, board)
    MovesGenerator(exposedPlayer.other, board)(isKingExposedToCheck)
      .exists(validMove =>
        !board.apply(validMove.convertInBoardMove).pieces.exists {
          case (_, Piece(color, pieceType)) => color == exposedPlayer && pieceType == King
      })
  }
}

object IsKingInCheckmate extends Check {
  private val isKingExposedToCheck = true

  /**
   * Checks if the king is in play, and then verifies if it is under checkmate
   * @param exposedPlayer the player who may be exposed to king checkmate
   * @param board in which calculate the check
   * @return true if the king is in checkmate
   */
  override def apply(exposedPlayer: Color, board: Board): Boolean = {
    requireExposedKing(exposedPlayer, board)
    generators.MovesGenerator(exposedPlayer, board)(isKingExposedToCheck).isEmpty
  }
}

abstract class Check extends ((Color, Board) => Boolean) {

  /**
   * Checks with Scala.Predef.require if the king exposed to check's control is in the board
   * @param exposedPlayer the player's color who may be exposed to check
   * @param board the board on which verify the check
   */
  protected def requireExposedKing(exposedPlayer: Color, board: Board): Unit =
    require(
      board.pieces
        .exists {
          case (_, Piece(color, pieceType)) => color == exposedPlayer && pieceType == King
        },
      "The " + exposedPlayer + " king doesn't exist in board (before verify the check)!!!"
    )

}
