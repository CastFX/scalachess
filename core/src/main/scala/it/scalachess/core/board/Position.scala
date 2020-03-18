package it.scalachess.core.board

import scala.math.abs

/**
 * Immutable representation of a Position on the board, represented by its row and column
 * @param row row number of the
 * @param col
 */
case class Position(row: Int, col: Int) {

  lazy val left: Option[Position]  = Position.of(row)(col - 1)
  lazy val right: Option[Position]    = Position.of(row)(col + 1)
  lazy val down: Option[Position]  = Position.of(row - 1)(col)
  lazy val up: Option[Position] = Position.of(row + 1)(col)

  lazy val downLeft: Option[Position]  = Position.of(row - 1)(col - 1)
  lazy val downRight: Option[Position]  = Position.of(row - 1)(col + 1)
  lazy val upLeft: Option[Position]    = Position.of(row + 1)(col - 1)
  lazy val upRight: Option[Position]    = Position.of(row + 1)(col + 1)



  /**
   * Checks if a certain position touches this position
   * @param pos the other position
   * @return true if the positions are adjacent
   */
  def isAdjacentTo(pos: Position): Boolean = xDistanceTo(pos) == 1 || xDistanceTo(pos) == 1

  /**
   * Checks if there is a diagonal path between this and the other position
   * @param pos the other position
   * @return true if the positions are in a diagonal path
   */
  def isDiagonalTo(pos: Position): Boolean = xDistanceTo(pos) == yDistanceTo(pos)


  /**
   * Checks if there is a straight path between this and the other position
   * @param pos the other position
   * @return true if the positions are in a straight path
   */
  def isStraightTo(pos: Position): Boolean = xDistanceTo(pos) == 0 || yDistanceTo(pos) == 0

  /**
   * Computes the absolute value of the difference between the row values
   * @param pos the other position
   * @return A positive Int representing the distance between the rows
   */
  def xDistanceTo(pos: Position): Int = abs(row - pos.row)

  /**
   * Computes the absolute value of the difference between the column values
   * @param pos the other position
   * @return A positive Int representing the distance between the columns
   */
  def yDistanceTo(pos: Position): Int = abs(col - pos.col)
}

object Position {

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param row numerical value of the position's row
   * @param col column of the new position
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def of(row: Int) (col: Int): Option[Position] = {
    if (Board.isInside(row,col)) Option(Position(row,col))
    else None
  }

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param row letter of the new position's row
   * @param col column of the new position
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def of(row: Char) (col: Int): Option[Position] = {
    val rowAsInt = row.toLower.toInt - 96
    Position.of(rowAsInt)(col)
  }
}
