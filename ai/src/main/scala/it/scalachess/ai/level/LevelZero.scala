package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import scalaz.{ Failure, Success, Validation }

import scala.util.Random

/**
 * The level zero AI plays just random moves
 */
case class LevelZero() extends Level {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] = {
    val moves = new MoveGenerator(board: Board, aiPlayer: Color, history: Seq[FullMove]).allMoves()
    if (moves.nonEmpty)
      Success(moves(Random.nextInt(moves.size)))
    else
      Failure(aiInCheckmateFailMsg)
  }

}
