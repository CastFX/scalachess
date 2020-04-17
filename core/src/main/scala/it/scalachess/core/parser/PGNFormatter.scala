package it.scalachess.core.parser

import it.scalachess.core.{ Black, Color, Draw, Result, White, Win, WinByForfeit }
import scalaz.Validation

trait PGNFormatter[B] {
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
