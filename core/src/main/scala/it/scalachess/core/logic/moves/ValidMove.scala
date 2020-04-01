package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.Position
import it.scalachess.core.pieces.{ Piece, PieceType }

sealed trait ValidMove {
  def convertInBoardMove: BoardMove

}
case class ValidSimpleMove(pieceType: PieceType,
                           color: Color,
                           from: Position,
                           to: Position,
                           capturedPiece: Option[Piece])
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardSimpleMove(from, to)
}

case class ValidCastling(castlingType: CastlingType,
                         kingPos: Position,
                         rookPos: Position,
                         kingFinalPos: Position,
                         rookFinalPos: Position)
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardCastling(kingPos, rookPos, kingFinalPos, rookFinalPos)
}

case class ValidEnPassant(from: Position, to: Position, capturePos: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardEnPassant(from, to, capturePos)
}

case class ValidPromotion(from: Position, to: Position, piece: Piece) extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardPromotion(from, to, piece)
}
