package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import scalaz.{ Failure, Success, Validation }

trait Level extends ((Board, Color, Seq[FullMove]) => Validation[String, FullMove]) {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove]

  protected def moveWithMaxEvaluation(movesEvaluated: Map[FullMove, Double]): Validation[String, FullMove] =
    if (movesEvaluated.isEmpty) Failure(AIinCheckmateFailMsg)
    else {
      Success(movesEvaluated.find(_._2 == movesEvaluated.values.max) match {
        case Some(maxEvalEntry) => maxEvalEntry._1
        case _                  => movesEvaluated.head._1
      })
    }
  protected val AIinCheckmateFailMsg = "The AI player is on checkmate, the game should be already ended"
}
