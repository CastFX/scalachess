package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.Board.BoardChanges
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.pieces.{ King, Pawn, Piece, PieceType, Rook }

/**
 * Definition of a ValidMove
 */
sealed trait ValidMove {
  val from: Position
  val to: Position
  val pieceType: PieceType
  val color: Color
  val capture: Option[Position]
  val boardChanges: BoardChanges
}

/**
 * Definition of a ValidMove of a piece
 * @param from the starting position of the piece
 * @param to the ending position of the piece
 * @param pieceType the type of the piece
 * @param color the color of the piece
 * @param capture true if the move does a capture, false otherwise
 */
final case class ValidSimpleMove(from: Position,
                                 to: Position,
                                 pieceType: PieceType,
                                 color: Color,
                                 capture: Option[Position])
    extends ValidMove {

  val boardChanges: BoardChanges =
    BoardChanges(
      updates = Map(to -> Piece(color, pieceType)),
      deletions = Set(from)
    )
}

/**
 * Definition of a valid en passant
 * @param from the starting position of the piece
 * @param to the ending position of the piece
 * @param color the color of the piece
 * @param capturedPawn the pawn that is captured after the piece is moved
 */
final case class ValidEnPassant(from: Position, to: Position, color: Color, capturedPawn: Position) extends ValidMove {

  val pieceType: PieceType      = Pawn
  val capture: Option[Position] = Some(capturedPawn)
  val boardChanges: BoardChanges =
    BoardChanges(
      updates = Map(to -> Piece(color, pieceType)),
      deletions = Set(from, capturedPawn)
    )
}

/**
 * Definition of a valid promotion
 * @param from the starting position of the piece
 * @param to the ending position of the piece
 * @param color the color of the piece
 * @param promotesTo the piece that the starting piece should be promoted to
 * @param capture the position of the piece to capture if the move results in a capture
 */
final case class ValidPromotion(from: Position,
                                to: Position,
                                color: Color,
                                promotesTo: Piece,
                                capture: Option[Position])
    extends ValidMove {

  val pieceType: PieceType = Pawn
  val boardChanges: BoardChanges =
    BoardChanges(
      updates = Map(to -> promotesTo),
      deletions = Set(from)
    )
}

/**
 * Definition of a valid Castling
 * @param from the starting position of the King
 * @param to the ending position of the King
 * @param color the color of the King
 * @param rookFrom the starting position of the Rook
 * @param rookTo the ending position of the Rook
 * @param castlingType the type of the Castling: QueenSide or KingSide
 */
final case class ValidCastling(from: Position,
                               to: Position,
                               color: Color,
                               rookFrom: Position,
                               rookTo: Position,
                               castlingType: CastlingType)
    extends ValidMove {

  val pieceType: PieceType      = King
  val capture: Option[Position] = None
  val boardChanges: BoardChanges =
    BoardChanges(
      updates = Map(to -> Piece(color, King), rookTo -> Piece(color, Rook)),
      deletions = Set(from, rookFrom)
    )
}

/**
 * Definition of the possible types of castling
 */
sealed trait CastlingType

/**
 * Definition of a King Side castling, also called short castling
 */
case object KingSide extends CastlingType

/**
 * Definition of a Queen Side castling, also called long castling
 */
case object QueenSide extends CastlingType

/**
 * Definition of a move that contains a ValidMove with the information related to check and checkmate.
 * @param validMove the validMove
 * @param resultsInCheck true if the move results in a check, false otherwise
 * @param resultsInCheckmate the if the move results in a checkmate, false otherwise
 */
final case class FullMove(validMove: ValidMove, resultsInCheck: Boolean, resultsInCheckmate: Boolean, boardAfter: Board)
