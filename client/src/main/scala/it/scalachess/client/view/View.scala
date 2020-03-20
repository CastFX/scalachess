package it.scalachess.client.view

import com.typesafe.scalalogging.Logger
import it.scalachess.core.board.Board
import scala.collection.immutable.NumericRange

/**
 * Representation of a standard view
 */
trait View {

  /**
   * Shows the game board
   * @param board a game board
   */
  def showBoard(board: Board)

  /**
   * Returns a representation in String of the board
   * @param board a game board
   * @return the representation of the board in a string
   */
  def getStringFromBoard(board: Board): String
}

/**
 * An implementation of a CLI view
 */
class CliView extends View {
  val logger: Logger  = Logger("ViewLogger")
  val newLine: String = "\n"

  override def showBoard(board: Board): Unit = logger.debug(newLine + getStringFromBoard(board))

  override def getStringFromBoard(board: Board): String = {
    val lettersRange: NumericRange.Inclusive[Char] = 'A' to (Board.width + 64).toChar
    val emptyCell: String                          = "\u3000"
    val cell: String                               = "[ %s ]"
    val letterCell: String                         = "\u2007 %s \u3000"

    val boardString = {
      (Board.height to 1 by -1)
        .map { row =>
          row + " " +
          lettersRange.map { column =>
            board.pieceAt(s"$column$row") match {
              case None        => cell.format(emptyCell)
              case Some(piece) => cell.format(piece.symbol)
            }
          }.mkString
        }
        .mkString(newLine)
    }
    boardString.concat(newLine + emptyCell + lettersRange.map(letterCell.format(_)).mkString)
  }
}
