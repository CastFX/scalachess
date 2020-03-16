package it.scalachess.core.board

import scala.math.abs

case class Position(x: Int, y: Int) {

  val down: Option[Position] = Position.of(x, y - 1)
  val up: Option[Position] = Position.of(x, y + 1)
  val left: Option[Position] = Position.of(x - 1, y)
  val right: Option[Position] = Position.of(x + 1, y)

  def isAdjacentTo(pos: Position): Boolean = xDistanceTo(pos) == 1 || xDistanceTo(pos) == 1
  def isDiagonalTo(pos: Position): Boolean = xDistanceTo(pos) == yDistanceTo(pos)
  def isStraightTo(pos: Position): Boolean = xDistanceTo(pos) == 0 || yDistanceTo(pos) == 0

  def xDistanceTo(pos: Position): Int = abs(x - pos.x)
  def yDistanceTo(pos: Position): Int = abs(y - pos.y)
}

object Position {
  def apply(x: Int, y: Int): Position = {
    require(Board.isInside(x,y))
    Position(x,y)
  }

  def of(x: Int, y: Int): Option[Position] = {
    if (Board.isInside(x,y)) Option(Position(x,y))
    else None
  }
}