package it.scalachess.core

sealed trait GameStatus
case object Ongoing           extends GameStatus
case object Draw              extends GameStatus
case class Win(player: Color) extends GameStatus
