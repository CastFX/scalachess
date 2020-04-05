package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.logic.moves.{ AlgebraicCastling, AlgebraicMove, AlgebraicSimpleMove, FullMove }
import it.scalachess.core.logic.moves.ListFullMoveExtension.ListFullMove
import scalaz.{ Failure, Success, Validation }

/**
 * It can be used to validate a move
 */
object MoveValidator {

  /**
   * Validates a move
   * @param board the board where the move should be valid
   * @param player the player trying to make the move
   * @param history the history of the moves of the chess game
   * @param move the move the player is trying to make
   * @return a Failure if the move is not valid, a FullMove otherwise
   */
  def apply(board: Board, player: Color, history: Seq[FullMove])(move: AlgebraicMove): Validation[String, FullMove] = {
    val fullMoves: List[FullMove] = new MoveGenerator(board, player, history)
      .allMoves()

    val equivalentMoves: List[FullMove] = move match {
      case AlgebraicCastling(castlingType, check, checkmate) =>
        fullMoves
          .filterCastlings(castlingType)
          .filterChecks(check, checkmate)
      case AlgebraicSimpleMove(endPos, pieceType, capture, check, checkmate, startingCol, startingRow, promotion) =>
        fullMoves
          .filterPieces(pieceType)
          .filterPositions(endPos, startingCol, startingRow)
          .filterChecks(check, checkmate)
          .filterCaptures(capture)
          .filterPromotions(promotion)
    }

    equivalentMoves.headOption match {
      case _ if equivalentMoves.size > 1 => Failure("This move creates an ambiguity, please specify it better")
      case Some(move)                    => Success(move)
      case None                          => Failure("The move is not a valid one")
    }
  }
}
