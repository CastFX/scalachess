package it.scalachess.core.parser

import it.scalachess.core.logic.{ Draw, Result, Win, WinByForfeit }
import it.scalachess.core.{ Black, Color, White }
import scalaz.Validation

/**
 * Formatter that takes a sequence of Validation obtained from a parser and format it into a string representing the game
 * @tparam B the type of the return of the parser
 */
trait PGNFormatter[B] {

  /**
   * Given the input, it returns a representation of the game
   * @param moves the sequence of validation returned by the parser representing the moves of a game
   * @param result the result of the game after applying the moves
   * @return a string that represents the game
   */
  def format(moves: Seq[Validation[String, B]], result: Option[Result]): String =
    (for (group <- moves.flatMap(_.toOption).grouped(2))
      yield group.mkString("", " ", "\n")).zipWithIndex
      .map {
        case (moves: String, index: Int) => s"${index + 1}.$moves"
      }
      .mkString
      .concat(gameEnd(result))

  private def gameEnd(result: Option[Result]): String =
    result match {
      case Some(Draw)                 => "1/2\n"
      case Some(Win(player))          => winner(player)
      case Some(WinByForfeit(player)) => winner(player)
      case _                          => ""
    }

  private def winner(color: Color): String =
    color match {
      case White => "1-0\n"
      case Black => "0-1\n"
    }
}
