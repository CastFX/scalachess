package it.scalachess.client.view

import com.typesafe.scalalogging.Logger
import it.scalachess.core.White
import it.scalachess.core.board.Board
import it.scalachess.core.pieces.{ Bishop, King, Knight, Piece, PieceType, Queen, Rook }

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
object CliView extends View {
  val logger: Logger  = Logger("ViewLogger")
  val newLine: String = "\n"

  override def showBoard(board: Board): Unit = logger.debug(newLine + getStringFromBoard(board))

  def cliVisualization(piece: Piece): String = {
    val result: String = piece.pieceType match {
      case King   => "K"
      case Queen  => "Q"
      case Bishop => "B"
      case Rook   => "R"
      case Knight => "N"
      case _      => "P"
    }
    if (!isWhite(piece)) result.toLowerCase() else result
  }

  def isWhite(piece: Piece): Boolean =
    piece.color == White

  override def getStringFromBoard(board: Board): String = {
    val lettersRange: NumericRange.Inclusive[Char] = 'A' to (Board.width + 64).toChar
    val emptyCell: String                          = " "
    val cell: String                               = "[ %s ]"
    val letterCell: String                         = "  %s  "

    val boardString = {
      (Board.height to 1 by -1)
        .map { row =>
          s"$row " +
          lettersRange.map { column =>
            board.pieceAt(s"$column$row") match {
              case None        => cell.format(emptyCell)
              case Some(piece) => cell.format(cliVisualization(piece))
            }
          }.mkString
        }
        .mkString(newLine)
    }
    boardString.concat(newLine + emptyCell + emptyCell + lettersRange.map(letterCell.format(_)).mkString)
  }
}
