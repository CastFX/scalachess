package it.scalachess.client.view

import com.typesafe.scalalogging.Logger
import it.scalachess.core.{ Draw, Result, White, Win, WinByForfeit }
import it.scalachess.core.board.Board
import it.scalachess.core.pieces.{ Bishop, King, Knight, Piece, Queen, Rook }

import scala.collection.immutable.NumericRange

/**
 * Representation of a standard view
 */
trait View {

  /**
   * Shows the game board
   * @param board the board to be shown
   */
  def showBoard(board: Board)

  /**
   * Shows a message.
   * @param message the message to be shown
   */
  def showMessage(message: String)

  /**
   * Shows a result. (Win, Forfeit or Draw)
   * @param result the result to be shown
   */
  def showResult(result: Result)
}

/**
 * An implementation of a CLI view
 */
object CliView extends View {
  val logger: Logger  = Logger("ViewLogger")
  val newLine: String = "\n"

  override def showBoard(board: Board): Unit = logger.info(newLine + getStringFromBoard(board))

  def showMessage(message: String): Unit = logger.info(message)

  def showResult(result: Result): Unit = result match {
    case Win(player)          => logger.info(s"$player wins")
    case WinByForfeit(player) => logger.info(s"$player wins by forfeit")
    case Draw                 => logger.info("Draw")
  }

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

  private def getStringFromBoard(board: Board): String = {
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
