package it.scalachess.core.gamestatus

import it.scalachess.core.Color

sealed trait GameStatus
case object Ongoing           extends GameStatus
case object Draw              extends GameStatus
case class Win(player: Color) extends GameStatus
