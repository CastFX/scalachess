package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.MovesGenerator
import it.scalachess.core.logic.moves.{ AlgebraicMove, BoardMove, ValidMove }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  /**
   * Checks if a parsedMove is also a valid move and can be applied to a board.
   * @param move the parsed move to be validated
   * @param player the player which is doing the move
   * @return Success(BoardMove) if the move is valid, a Failure message otherwise
   */
  def validateParsedMove(move: AlgebraicMove, player: Color): Validation[String, BoardMove] = {
    val errorMessage: String        = "The move is not a valid one"
    val ambiguityMessage: String    = "This move creates an ambiguity, please specify it better"
    val isKingCheck: Boolean        = true
    val validMoves: List[ValidMove] = MovesGenerator(player, board)(isKingCheck)
    println(validMoves)
    val parsedMoves: List[AlgebraicMove] = validMoves
      .map(validMove => validMove.convertInParsedMove(board))
    val map: Map[ValidMove, AlgebraicMove] = (validMoves zip parsedMoves).toMap
    println(map)
    println(move)
    val filteredMap = map.filter(parsed => move.isEqualTo(parsed._2))
    filteredMap.size match {
      case 0 => Failure(errorMessage)
      case 1 => Success(filteredMap.head._1.convertInBoardMove)
      case _ => Failure(ambiguityMessage)
    }
  }
}
