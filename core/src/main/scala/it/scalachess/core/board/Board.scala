package it.scalachess.core.board

import it.scalachess.core.logic.moves.{ BoardCastling, BoardEnPassant, BoardMove, BoardPromotion, BoardSimpleMove }
import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }

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
   * @return the board with changes
   */
  def apply(boardMove: BoardMove): Board =
    boardMove match {
      case BoardPromotion(from, to, pieceToRestore) =>
        require(pieceAtPosition(from).nonEmpty,
                "Applying Promotion move on board: the piece to move doesn't exist in board")
        executeSimpleMove(from, to, pieceToRestore)
      case BoardCastling(kingPos, rookPos, kingFinalPos, rookFinalPos) =>
        pieceAtPosition(kingPos) match {
          case None =>
            require(requirement = false,
                    "Applying castling move on board: the first piece to move doesn't exist in board")
            this
          case Some(firstPieceToMove) =>
            pieceAtPosition(rookPos) match {
              case None =>
                require(requirement = false,
                        "Applying EnPassant move on board: the second piece to capture doesn't exist in board")
                this
              case Some(secondPieceToMove) =>
                executeSimpleMove(kingPos, kingFinalPos, firstPieceToMove)
                  .executeSimpleMove(rookPos, rookFinalPos, secondPieceToMove)
            }
        }
      case BoardEnPassant(from, to, capturePos) =>
        pieceAtPosition(from) match {
          case None =>
            require(requirement = false, "Applying EnPassant move on board: the piece to move doesn't exist in board")
            this
          case Some(pieceToMove) =>
            require(pieceAtPosition(capturePos).nonEmpty,
                    "Applying EnPassant move on board: the piece to capture doesn't exist in board")
            executeSimpleMove(from, to, pieceToMove)
              .removePieceAtPosition(capturePos)
        }

      case BoardSimpleMove(from, to) =>
        pieceAtPosition(from) match {
          case None =>
            require(requirement = false, "Applying SimpleMove move on board: the piece to move doesn't exist in board")
            this
          case Some(pieceToMove) => executeSimpleMove(from, to, pieceToMove)
        }

    }

  private def executeSimpleMove(from: Position, to: Position, pieceToMove: Piece): Board =
    Board(pieces + (to -> pieceToMove) - from)

  private def removePieceAtPosition(pos: Position): Board =
    Board(pieces - pos)

}

object Board {
  val width: Int                  = 8
  val height: Int                 = 8
  val whiteMajorPiecesStartingRow = 1
  val whitePawnsStartingRow       = 2
  val blackMajorPiecesStartingRow = 8
  val blackPawnsStartingRow       = 7

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
          .of(col, row) //from the position
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
