package it.scalachess.core.board

import it.scalachess.core.logic.moves.{ BoardCastling, BoardEnPassant, BoardMove, BoardPromotion, BoardSimpleMove }
import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

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
   * @param col numerical value of column
   * @param row number of the row
   * @return an Option[Piece] with the respective Piece at that coordinates, None if it's empty
   */
  def pieceAtCoordinates(col: Int)(row: Int): Option[Piece] = Position.ofCurr(row)(col) flatMap { pieces get }

  /**
   * Retrieves a Piece under a certain position, if it's present
   * @param notation String notation of the Position, e.g "a1", "h8"
   * @return an Option[Piece] with the respective Piece at those coordinates, None if there isn't any
   */
  def pieceAt(notation: String): Option[Piece] = Position.ofNotation(notation) flatMap { pieces get }

  /**
   * Applies a move on the board. There is a strong assumption in this method:
   * it doesn't perform any logic check, it just make sures that the arguments
   * passed as input exists in its domain, then executes the related changes
   * @param boardMove containing the information for change the board
   * @return
   */
  def apply(boardMove: BoardMove): Validation[String, Board] =
    boardMove match {
      case BoardPromotion(from, to, pieceToRestore) => applyPromotion(from, to, pieceToRestore)
      case BoardCastling(kingPos, rookPos)          => applyCastling(kingPos, rookPos)
      case BoardEnPassant(capturePos, from, to)     => applyEnPassant(capturePos, from, to)
      case BoardSimpleMove(from, to) =>
        pieceAtPosition(to) match {
          case Some(pieceCaptured) =>
            Success(Board(pieces + (to -> pieceAtPosition(to).get) - from))
          case None => Success(Board(pieces + (to -> pieceAtPosition(to).get) - from))
        }
      case _ => Failure("board has received a not valid move")
    }

  /**
   * Applies the promotion:
   * move the piece at position 'from' to the position 'to';
   * adds the piece located at position 'pos' into the 'capturedPieces';
   * puts the 'pieceToRestore' from 'capturedPieces' to position 'pos'
   * @param from the position in which the piece starts the movement
   * @param to the position in which the piece on board receives the promotion
   * @param pieceToRestore the piece which return on board
   * @return
   */
  private def applyPromotion(from: Position, to: Position, pieceToRestore: Piece): Validation[String, Board] =
    pieceAtPosition(from) match {
      case None => Failure("applyPromotion - there's no piece at position received")
      case Some(_) =>
        Success(Board(pieces + (to -> pieceToRestore) - from))
    }

  private def applyCastling(kingPos: Position, rookPos: Position): Validation[String, Board] =
    pieceAtPosition(kingPos) match {
      case None => Failure("applyCastling - there's no piece at king's position received")
      case Some(kingPiece) =>
        pieceAtPosition(rookPos) match {
          case None => Failure("applyCastling - there's no piece at rook's position received")
          case Some(rookPiece) =>
            Success(Board(pieces + (rookPos -> kingPiece) + (kingPos -> rookPiece)))
        }
    }

  private def applyEnPassant(capturePos: Position, from: Position, to: Position) =
    pieceAtPosition(from) match {
      case None => Failure("applyEnPassant - there's no piece at the position received")
      case Some(pieceToMove) =>
        pieceAtPosition(capturePos) match {
          case None => Failure("applyEnPassant - there's no piece at the capture position")
          case Some(pieceCaptured) =>
            Success(Board(pieces + (to -> pieceToMove) - from - capturePos))
        }
    }

}

object Board {
  val whiteMajorPiecesStartingRow = 1
  val whiteMinorPiecesStartingRow = 2
  val blackMajorPiecesStartingRow = 8
  val blackMinorPiecesStartingRow = 7
  val kingsStartingCol            = 5
  val rightRooksStartingCol       = 8
  val leftRooksStartingCol        = 1
  val width: Int                  = 8
  val height: Int                 = 8

  /**
   * Function to check if a certain position expressed with row and column is inside this board
   * @param row the position's row to be checked
   * @param col the position's column to be checked
   * @return True if this position is within the Board
   */
  def isInside(col: Int, row: Int): Boolean =
    col >= 1 && col <= width && row >= 1 && row <= height

  /**
   * @return the standard 8x8 chess Board with pieces placed in the starting position
   */
  def defaultBoard(): Board = {
    val pieceMap = {
      for (row <- Seq(1, 2, height - 1, height); //for rows 1,2 7,8
           col <- 1 to 8) yield { //for each column [a-h]
        Position
          .ofCurr(col)(row) //from the position
          .map({ pos =>
            val color: Color = if (row <= 2) White else Black
            val piece        = Piece(color, initialPieceTypeAtPosition(pos)) //get the starting piece
            (pos, piece) //in a tuple
          })
      }
    }.flatten.toMap

    Board(pieceMap)
  }

  private def initialPieceTypeAtPosition(pos: Position): PieceType =
    pos.row match {
      case 1 | 8 =>
        pos.col match {
          case 1 | 8 => Rook //(a1,a8,h1,h8)
          case 2 | 7 => Knight //(b1,b8,g1,g8)
          case 3 | 6 => Bishop //(c1,c8,f1,f8)
          case 5     => King //(d1,d8)
          case 4     => Queen //(e1,e8)
        }
      case 2 | 7 => Pawn // (a2-h2, a7-h7)
    }
}
