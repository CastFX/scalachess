package it.scalachess.core.board

import scala.math.abs

/**
 * Immutable representation of a Position on the board, represented by its row and column
 * @param col number of the column
 * @param row number of row
 */
final case class Position(col: Int, row: Int) {

  lazy val left: Option[Position]  = Position.of(col - 1)(row)
  lazy val right: Option[Position] = Position.of(col + 1)(row)
  lazy val down: Option[Position]  = Position.of(col)(row - 1)
  lazy val up: Option[Position]    = Position.of(col)(row + 1)

  lazy val downLeft: Option[Position]  = Position.of(col - 1)(row - 1)
  lazy val downRight: Option[Position] = Position.of(col + 1)(row - 1)
  lazy val upLeft: Option[Position]    = Position.of(col - 1)(row + 1)
  lazy val upRight: Option[Position]   = Position.of(col + 1)(row + 1)

  /**
   * Computes the absolute value of the difference between the row values
   * @param pos the other position
   * @return A positive Int representing the distance between the rows
   */
  def rowDistance(pos: Position): Int = abs(row - pos.row)

  /**
   * Computes the effective value of the difference between the row values
   * @param pos the other position
   * @return An Int representing the substraction between the two rows
   * */
  def effectiveRowDistance(pos: Position): Int = row - pos.row

  /**
   * Computes the absolute value of the difference between the column values
   * @param pos the other position
   * @return A positive Int representing the distance between the columns
   */
  def colDistance(pos: Position): Int = abs(col - pos.col)

  /**
   * Computes the effective value of the difference between the column values
   * @param pos the other position
   * @return An Int representing the substraction between the two columns
   * */
  def effectiveColDistance(pos: Position): Int = col - pos.col

  /**
   * Checks if a certain position touches this position
   * @param pos the other position
   * @return true if the positions are adjacent
   */
  def isAdjacentTo(pos: Position): Boolean = {
    val (colD: Int, rowD: Int) = (colDistance(pos), rowDistance(pos))
    colD == 1 || rowD == 1
  }

  /**
   * Checks if there is a diagonal path between this and the other position
   * @param pos the other position
   * @return true if the positions are in a diagonal path
   */
  def isDiagonalTo(pos: Position): Boolean = rowDistance(pos) == colDistance(pos)

  /**
   * Checks if there is a straight path between this and the other position
   * @param pos the other position
   * @return true if the positions are in a straight path
   */
  def isStraightTo(pos: Position): Boolean = rowDistance(pos) == 0 || colDistance(pos) == 0

  override def toString: String = s"${col.toChar}$row"
}

object Position {

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param col column of the new position
   * @param row numerical value of the position's row
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def of(col: Int)(row: Int): Option[Position] =
    if (Board.isInside(col, row)) Option(Position(col, row))
    else None

  /**
   * Creates a valid position, only if both row and column are inside the board
   * @param notation chess notation of the position in the board
   * @return Option of the new Position if it's inside the board, None otherwise
   */
  def ofNotation(notation: String): Option[Position] =
    if (notation.length == 2 && notation.charAt(0).isLetter && notation.charAt(1).isDigit) {
      val col: Int = notation.charAt(0).toLower.toInt - 96
      val row: Int = notation.charAt(1).asDigit
      Position.of(col)(row)
    } else None
}
