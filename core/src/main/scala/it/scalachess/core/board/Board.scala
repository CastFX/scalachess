package it.scalachess.core.board

import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }
import it.scalachess.core.colors.{ Black, Color, White }

/**
 * Functional chess board representation
 * @param pieces a map Position -> Piece.
 */
final case class Board(
    pieces: Map[Position, Piece]
) {

  /**
   * Returns an Option type of the Piece at a certain Position, if present
   * @param pos A position on the Board
   * @return an Option[Piece] with the respective Piece on that Position, None if it's empty
   */
  def pieceAtPosition(pos: Position): Option[Piece] = pieces get pos

  /**
   * Curried function of pieceAt, to get the Piece at the passed coordinates
   * @param row numerical value of the row
   * @param col column number
   * @return an Option[Piece] with the respective Piece at that coordinates, None if it's empty
   */
  def pieceAtCoordinates(row: Int)(col: Int): Option[Piece] = Position.of(row)(col) flatMap { pieces get }

  /**
   * Curried function of pieceAt, to get the Piece at the passed coordinates
   * @param row row letter
   * @param col column number
   * @return an Option[Piece] with the respective Piece at that coordinates, None if it's empty
   */
  def pieceAt(row: Char)(col: Int): Option[Piece] = Position.ofNotation(row)(col) flatMap { pieces get }

}

object Board {
  val width: Int  = 8
  val height: Int = 8

  /**
   * Function to check if a certain position expressed with row and column is inside this board
   * @param row the position's row to be checked
   * @param col the position's column to be checked
   * @return True if this position is within the Board
   */
  def isInside(row: Int, col: Int): Boolean =
    row >= 1 && row <= width && col >= 1 && col <= height

  /**
   * @return the standard 8x8 chess Board with pieces placed in the starting position
   */
  @SuppressWarnings(Array("org.wartremover.warts.Option2Iterable"))
  def defaultBoard(): Board = {
    val pieceMap = {
      for (i <- Seq(1, 2, height - 1, height);
           j <- 1 to 8) yield {
        Position
          .of(i)(j)
          .map({ pos =>
            val color: Color = if (i <= 2) White else Black
            val piece        = Piece(color, initialPieceTypeAtPosition(pos))
            (pos, piece)
          })
      }
    }.flatten.toMap

    Board(pieceMap)
  }

  private def initialPieceTypeAtPosition(pos: Position): PieceType =
    pos.row match {
      case 1 | 8 =>
        pos.col match {
          case 1 | 8 => Rook
          case 2 | 7 => Knight
          case 3 | 6 => Bishop
          case 4     => King
          case 5     => Queen
        }
      case 2 | 7 => Pawn
    }
}
