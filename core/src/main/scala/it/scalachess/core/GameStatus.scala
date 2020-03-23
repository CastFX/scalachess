package it.scalachess.core.gamestatus

sealed trait GameStatus

final case object Ongoing  extends GameStatus
final case object Draw     extends GameStatus
final case object WhiteWin extends GameStatus
final case object BlackWin extends GameStatus
