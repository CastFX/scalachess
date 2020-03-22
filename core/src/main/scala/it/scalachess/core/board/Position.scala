package it.scalachess.core.board

import scala.annotation.tailrec
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
   *
   * @param pos the other position
   * @return A positive Int representing the distance between the rows
   */
  def rowDistanceAbs(pos: Position): Int = abs(row - pos.row)

  /**
   * Computes the integer value of the difference between the row values
   *
   * @param pos the other position
   * @return An Int representing the substraction between the two rows
   **/
  def rowDistanceInt(pos: Position): Int = row - pos.row

  /**
   * Computes the absolute value of the difference between the column values
   *
   * @param pos the other position
   * @return A positive Int representing the distance between the columns
   */
  def colDistanceAbs(pos: Position): Int = abs(col - pos.col)

  /**
   * Computes the integer value of the difference between the column values
   *
   * @param pos the other position
   * @return An Int representing the substraction between the two columns
   **/
  def colDistanceInt(pos: Position): Int = col - pos.col

  /**
   * Checks if a certain position touches this position
   *
   * @param pos the other position
   * @return true if the positions are adjacent
   */
  def isAdjacentTo(pos: Position): Boolean =
    if (pos == up || pos == upRight || pos == right || pos == downRight ||
        pos == down || pos == downLeft || pos == left || pos == upLeft) true
    else false

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

  /**
   * Computes and get the col positions between the two position.
   * The two end point are not include in the computation.
   * @param pos the other position
   * @param path the List in which put the positions
   * @return List[Position]
   */
  @tailrec
  def computeColPosBetween(pos: Position, path: List[Position]): List[Position] = {
    val colDist = this colDistanceInt pos
    if (colDist == 1 || colDist == -1) path
    else {
      var colMod = 0
      if (colDist > 0) colMod = 1
      else if (colDist < 0) colMod = -1
      val posApproachingFromFar = Position(col + colMod, row)
      computeColPosBetween(posApproachingFromFar, path.::(posApproachingFromFar))
    }
  }

  /**
   * Computes and get the row positions between the two position.
   * The two end point are not include in the computation.
   * @param pos the other position
   * @param path the List in which put the positions
   * @return List[Position]
   */
  @tailrec
  def computeRowPosBetween(pos: Position, path: List[Position]): List[Position] = {
    val rowDist = this rowDistanceInt pos
    if (rowDist == 1 || rowDist == -1) path
    else {
      var rowMod = 0
      if (rowDist > 0) rowMod = 1
      else if (rowDist < 0) rowMod = -1
      val posApproachingFromFar = Position(col, row + rowMod)
      computeRowPosBetween(posApproachingFromFar, path.::(posApproachingFromFar))
    }
  }

  /**
   * Computes and get the diagonal positions between the two position.
   * The two end point are not include in the computation.
   * @param pos the other position
   * @param path the List in which put the positions
   * @return List[Position]
   */
  @tailrec
  def computeDiagonalPosBetween(pos: Position, path: List[Position]): List[Position] = {
    val rowDist = this rowDistanceInt pos
    val colDist = this colDistanceInt pos
    if (!(rowDist == 1 || colDist == 1 || rowDist == -1 || colDist == -1)) path
    else {
      var colMod = 0
      var rowMod = 0
      if (colDist > 0) colMod = 1
      else if (colDist < 0) colMod = -1
      if (rowDist > 0) rowMod = -1
      else if (rowDist < 0) rowMod = 1
      val posApproachingFromFar = Position(col + colMod, row + rowMod)
      computeDiagonalPosBetween(posApproachingFromFar, path.::(posApproachingFromFar))
    }
  }

  def computeAdjacentPos(): List[Position] =
    up.toList ++ upRight.toList ++ right.toList ++
    downRight ++ down ++ downLeft ++ left ++ upLeft

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
