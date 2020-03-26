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

  lazy val adjacentPositions =
    Set(left, right, down, up, downLeft, downRight, upLeft, upRight).flatten

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
  def rowDistance(pos: Position): Int = row - pos.row

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
  def colDistance(pos: Position): Int = col - pos.col

  /**
   * Checks if a certain position touches this position
   *
   * @param pos the other position
   * @return true if the positions are adjacent
   */
  def isAdjacentTo(pos: Position): Boolean = adjacentPositions contains pos

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
   * Generates the set of the straight path between columns of this and the other position.
   * The two position located at the end points are not include in the path.
   * @param pos the other position
   * @param path the Set in which put the positions
   * @return path
   */
  @tailrec
  def generatePosBetweenCol(pos: Position, path: Set[Position]): Set[Position] =
    pos colDistance this match {
      case adjacentDist if adjacentDist == 1 || adjacentDist == -1 || adjacentDist == 0 => path
      case colDist =>
        val approachingToThisPos = Position(pos.col + computePathModifier(colDist), pos.row)
        generatePosBetweenCol(approachingToThisPos, path + (approachingToThisPos))
    }

  /**
   * Generates the set of the straight path between rows of this and the other position.
   * The two position located at the end points are not include in the path.
   * @param pos the other position
   * @param path the Set in which put the positions
   * @return path
   */
  @tailrec
  def generatePosBetweenRow(pos: Position, path: Set[Position]): Set[Position] =
    pos rowDistance this match {
      case adjacentDist if adjacentDist == 1 || adjacentDist == -1 || adjacentDist == 0 => path
      case rowDist =>
        val approachingToThisPos = Position(pos.col, pos.row + computePathModifier(rowDist))
        generatePosBetweenRow(approachingToThisPos, path + (approachingToThisPos))
    }

  /**
   * Generates the set of the diagonal path between this and the other position.
   * The two position located at the end points are not include in the path.
   * @param pos the other position
   * @param path the Set in which put the positions
   * @return path
   */
  @tailrec
  def generatePosBetweenDiagonal(pos: Position, path: Set[Position]): Set[Position] =
    (pos colDistance this, pos rowDistance this) match {
      case adjacentDist
          if adjacentDist._1 == 1 || adjacentDist._1 == -1 || adjacentDist._1 == 0 ||
          adjacentDist._2 == 1 || adjacentDist._2 == -1 || adjacentDist._2 == 0 =>
        path
      case distances =>
        val approachingToThisPos =
          Position(pos.col + computePathModifier(distances._1), pos.row + computePathModifier(distances._2))
        generatePosBetweenDiagonal(approachingToThisPos, path + (approachingToThisPos))
    }

  /**
   * Calculates the modifier in the methods: generatePosBetweenCol, generatePosBetweenRow, computePosBetweenDiagonal
   * @param distance the distance between the this and the position to reach
   * @return the modifier
   */
  private def computePathModifier(distance: Int): Int =
    if (distance > 0) -1
    else if (distance < 0) 1
    else 0

  override def toString: String = s"${(col + 96).toChar}$row"
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
