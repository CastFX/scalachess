package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ IsKingInCheck, IsKingInCheckmate }
import it.scalachess.core.pieces.{ Piece, PieceType }

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
  override def convertInBoardMove: BoardMove = BoardSimpleMove(from, to)

  /**
   * Converts the ValidMove into a ParsedMove given a board.
   * @param board the current board
   * @return the ParsedMove obtained by converting the ValidMove
   */
  override def convertInParsedMove(board: Board): ParsedMove = {
    val nextBoard = board(convertInBoardMove)
    val captured =
      if (capturedPiece.isDefined) Capture(Some(pieceType), Some((to.col + 96).toChar)) else Capture(None, None)
    val check: Boolean     = IsKingInCheck(color.other, nextBoard)
    val checkmate: Boolean = IsKingInCheckmate(color.other, nextBoard)
    ParsedSimpleMove(to, pieceType, captured, check, checkmate, Some((from.col + 96).toChar), Some(from.row), None)
  }
}

case class ValidCastling(castlingType: CastlingType,
                         kingPos: Position,
                         rookPos: Position,
                         kingFinalPos: Position,
                         rookFinalPos: Position)
    extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardCastling(kingPos, rookPos, kingFinalPos, rookFinalPos)
  override def convertInParsedMove(board: Board): ParsedMove = ??? //Castling(castlingType, check, checkMate)
}

case class ValidEnPassant(from: Position, to: Position, capturePos: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardEnPassant(from, to, capturePos)
  override def convertInParsedMove(board: Board): ParsedMove = ???
}

case class ValidPromotion(from: Position, to: Position, piece: Piece) extends ValidMove {
  override def convertInBoardMove: BoardMove                 = BoardPromotion(from, to, piece)
  override def convertInParsedMove(board: Board): ParsedMove = ???
}
