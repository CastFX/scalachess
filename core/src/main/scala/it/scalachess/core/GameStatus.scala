package it.scalachess.core

import it.scalachess.core.colors.Color

sealed trait GameStatus
sealed trait Result
case object Ongoing                    extends GameStatus
case object Draw                       extends GameStatus with Result
case class Win(player: Color)          extends GameStatus with Result
case class WinByForfeit(player: Color) extends GameStatus with Result
