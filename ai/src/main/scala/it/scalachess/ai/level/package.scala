package it.scalachess.ai

import it.scalachess.core.logic.moves.FullMove
import scalaz.{ Failure, Success, Validation }

package object level {

  private[level] val aiInCheckmateFailMsg = "The AI player is on checkmate, the game should be already ended"

  private[level] def moveWithMaxEvaluation(movesEvaluated: Map[FullMove, Double]): Validation[String, FullMove] =
    if (movesEvaluated.isEmpty) Failure(aiInCheckmateFailMsg)
    else {
      Success(movesEvaluated.find(_._2 == movesEvaluated.values.max) match {
        case Some(maxEvalEntry) => maxEvalEntry._1
        case _                  => movesEvaluated.head._1
      })
    }

}
