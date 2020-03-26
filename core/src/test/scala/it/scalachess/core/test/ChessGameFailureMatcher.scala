package it.scalachess.core.test

import it.scalachess.core.ChessGame
import org.scalatest.matchers.{ MatchResult, Matcher }
import scalaz.{ Failure, Validation }

trait ChessGameFailureMatcher {

  class ChessGameEndInFailureMatcher() extends Matcher[Validation[String, ChessGame]] {
    def apply(game: Validation[String, ChessGame]) =
      game match {
        case Failure(errorMsg) =>
          MatchResult(true, "something goes wrong...", s"game has generate the Failure($errorMsg)")
        case _ =>
          MatchResult(false, "This game has generate Success instead of Failure", "something goes wrong...")
      }
  }

  def generateFailure = new ChessGameEndInFailureMatcher()
}

object ChessGameFailureMatcher extends ChessGameFailureMatcher
