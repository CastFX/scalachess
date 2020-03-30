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

case class ValidCastling(castlingType: CastlingType, kingPos: Position, rookPos: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardCastling(kingPos, rookPos)
}

case class ValidEnPassant(capture: Position, from: Position, to: Position) extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardEnPassant(capture, from, to)
}

case class ValidPromotion(from: Position, to: Position, piece: Piece) extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardPromotion(from, to, piece)
}
