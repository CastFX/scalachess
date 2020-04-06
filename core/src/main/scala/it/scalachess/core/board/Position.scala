package it.scalachess.core.board

import scala.math.abs

/**
 * Immutable representation of a Position on the board, represented by its row and column
 * @param col number of the column
 * @param row number of row
 */
final case class Position(col: Int, row: Int) {

  lazy val colLeftMod: Int  = -1
  lazy val colRightMod: Int = 1
  lazy val rowDownMod: Int  = -1
  lazy val rowUpMod: Int    = 1

  lazy val posLeft: Option[Position]  = Position.of(col + colLeftMod, row)
  lazy val posRight: Option[Position] = Position.of(col + colRightMod, row)
  lazy val posDown: Option[Position]  = Position.of(col, row + rowDownMod)
  lazy val posUp: Option[Position]    = Position.of(col, row + rowUpMod)

  lazy val posDownLeft: Option[Position]  = Position.of(col + colLeftMod, row + rowDownMod)
  lazy val posDownRight: Option[Position] = Position.of(col + colRightMod, row + rowDownMod)
  lazy val posUpLeft: Option[Position]    = Position.of(col + colLeftMod, row + rowUpMod)
  lazy val posUpRight: Option[Position]   = Position.of(col + colRightMod, row + rowUpMod)

  lazy val adjacentPositions: Set[Position] =
    Set(posLeft, posRight, posDown, posUp, posDownLeft, posDownRight, posUpLeft, posUpRight).flatten

  /**
   * Computes the absolute value of the difference between the row values
   *
   * @param pos the other position
   * @return A positive Int representing the distance between the rows
   */
  def rowDistanceAbs(pos: Position): Int = abs(row - pos.row)

  /**
   * Computes the absolute value of the difference between the column values
   *
   * @param pos the other position
   * @return A positive Int representing the distance between the columns
   */
  def colDistanceAbs(pos: Position): Int = abs(col - pos.col)

  /**
   * Checks if there is a diagonal path between this and the other position
   *
   * @param pos the other position
   * @return true if the positions are in a diagonal path
   */
  def isDiagonalTo(pos: Position): Boolean = rowDistanceAbs(pos) == colDistanceAbs(pos)

  /**
   * Checks if there is a straight path between this and the other position
   *
   * @param pos the other position
   * @return true if the positions are in a straight path
   */
  def isStraightTo(pos: Position): Boolean = rowDistanceAbs(pos) == 0 || colDistanceAbs(pos) == 0
}

object Position {

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param col column of the new position
   * @param row numerical value of the position's row
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def of(col: Int, row: Int): Option[Position] =
    if (Board.isInside(col, row)) Option(Position(col, row))
    else None

  def of(pos: Position): Option[Position] =
    if (Board.isInside(pos.col, pos.row)) Option(pos)
    else None

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param notation chess notation of the position in the board
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def ofNotation(notation: String): Option[Position] =
    if (notation.length == 2 && notation.charAt(0).isLetter && notation.charAt(1).isDigit) {
      val col: Int = colToInt(notation.charAt(0))
      val row: Int = notation.charAt(1).asDigit
      Position.of(col, row)
    } else None

  def colToInt(col: Char): Int =
    col.toLower.toInt - 96
}
