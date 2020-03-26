package it.scalachess.core.test

import it.scalachess.core.ChessGame
import org.scalatest.matchers.{ MatchResult, Matcher }
import scalaz.{ Failure, Validation }

trait ChessGameFailureMatcher {

  class ChessGameEndInFailureMatcher() extends Matcher[Validation[String, ChessGame]] {
    def apply(game: Validation[String, ChessGame]): MatchResult =
      game match {
        case Failure(errorMsg) =>
          MatchResult(matches = true, "something goes wrong...", s"game has generate the Failure($errorMsg)")
        case _ =>
          MatchResult(matches = false, "This game has generate Success instead of Failure", "something goes wrong...")
      }
  }

  def generateFailure: ChessGameEndInFailureMatcher = new ChessGameEndInFailureMatcher()
}

object ChessGameFailureMatcher extends ChessGameFailureMatcher
