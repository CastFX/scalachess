package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ CheckValidator, MoveValidator }
import it.scalachess.core.pieces.{ Piece, PieceType }
import scalaz.Success

sealed trait ValidMove {
  def convertInBoardMove: BoardMove
  def convertInParsedMove(board: Board): ParsedMove
}
case class ValidSimpleMove(pieceType: PieceType,
                           color: Color,
                           from: Position,
                           to: Position,
                           capturedPiece: Option[Piece])
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardSimpleMove(from, to, Piece(color, pieceType))
  override def convertInParsedMove(board: Board): ParsedMove = {
    val nextBoard = board(convertInBoardMove) match {
      case Success(a) => a
    }
    val captured = if (capturedPiece.isDefined) (Some(pieceType), Some(to.col.toChar)) else (None, None)
    val check: Boolean = CheckValidator().isKingInCheck(color, nextBoard) match {
      case Success(_) => true
      case _          => false
    }
    val checkmate: Boolean = CheckValidator().isKingInCheckmate(color, MoveValidator(nextBoard))
    ParsedSimpleMove(to, pieceType, captured, check, checkmate, Some((from.col + 96).toChar), Some(from.row), None)
  }
}

case class ValidCastling(castlingType: CastlingType, kingPos: Position, rookPos: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardCastling(kingPos, rookPos)
  override def convertInParsedMove(board: Board): ParsedMove = ??? //Castling(castlingType, check, checkMate)
}

case class ValidEnPassant(capture: Position, from: Position, to: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardEnPassant(capture, from, to)
  override def convertInParsedMove(board: Board): ParsedMove = ???
}

case class ValidPromotion(from: Position, to: Position, piece: Piece) extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardPromotion(from, to, piece)
  override def convertInParsedMove(board: Board): ParsedMove = ???
}
