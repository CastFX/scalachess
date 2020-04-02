package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MovesGenerator
import it.scalachess.core.logic.moves.{ AlgebraicMove, ValidMove }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  /**
   * Checks if a parsedMove is also a valid move and can be applied to a board.
   * @param move the parsed move to be validated
   * @param player the player which is doing the move
   * @return Success(BoardMove) if the move is valid, a Failure message otherwise
   */
  def validateAlgebraicMove(move: AlgebraicMove, player: Color): Validation[String, ValidMove] = {
    val isKingCheck: Boolean        = true
    val validMoves: List[ValidMove] = MovesGenerator(player, board)(isKingCheck)
    val algebraicMoves: List[AlgebraicMove] = validMoves
      .map(_.convertInAlgebraicMove(board))
    val map: Map[AlgebraicMove, ValidMove] = (algebraicMoves zip validMoves).toMap
    val filteredMap                        = map.filterKeys { move isEquivalentTo }
    filteredMap.values.headOption match {
      case _ if filteredMap.size > 1 => Failure("This move creates an ambiguity, please specify it better")
      case Some(validMove)           => Success(validMove)
      case None                      => Failure("The move is not a valid one")
    }
  }
}
