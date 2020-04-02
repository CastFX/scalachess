package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ IsKingInCheck, IsKingInCheckmate }
import it.scalachess.core.pieces.{ Piece, PieceType }

sealed trait ValidMove {
  def convertInBoardMove: BoardMove
  def convertInParsedMove(board: Board): AlgebraicMove
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
  override def convertInParsedMove(board: Board): AlgebraicMove =
    AlgebraicCreator.generateAlgebraicSimpleMove(board(convertInBoardMove),
                                                 capturedPiece,
                                                 pieceType,
                                                 color,
                                                 from,
                                                 to,
                                                 None)
}

case class ValidCastling(castlingType: CastlingType,
                         kingPos: Position,
                         rookPos: Position,
                         kingFinalPos: Position,
                         rookFinalPos: Position,
                         color: Color)
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardCastling(kingPos, rookPos, kingFinalPos, rookFinalPos)
  override def convertInParsedMove(board: Board): AlgebraicMove = {
    val nextBoard = board(convertInBoardMove)
    AlgebraicCastling(castlingType, IsKingInCheck(color.other, nextBoard), IsKingInCheckmate(color.other, nextBoard))
  }
}

case class ValidEnPassant(pieceType: PieceType,
                          color: Color,
                          from: Position,
                          to: Position,
                          capturedPiece: Option[Piece],
                          promotion: Option[Piece],
                          capturePos: Position)
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardEnPassant(from, to, capturePos)
  override def convertInParsedMove(board: Board): AlgebraicMove =
    AlgebraicCreator.generateAlgebraicSimpleMove(board(convertInBoardMove),
                                                 capturedPiece,
                                                 pieceType,
                                                 color,
                                                 from,
                                                 to,
                                                 promotion)
}

case class ValidPromotion(pieceType: PieceType,
                          color: Color,
                          from: Position,
                          to: Position,
                          capturedPiece: Option[Piece],
                          promotion: Piece)
    extends ValidMove {
  override def convertInBoardMove: BoardMove = BoardPromotion(from, to, promotion)
  override def convertInParsedMove(board: Board): AlgebraicMove =
    AlgebraicCreator.generateAlgebraicSimpleMove(board(convertInBoardMove),
                                                 capturedPiece,
                                                 pieceType,
                                                 color,
                                                 from,
                                                 to,
                                                 Some(promotion))
}

private object AlgebraicCreator {
  def generateAlgebraicSimpleMove(nextBoard: Board,
                                  capturedPiece: Option[Piece],
                                  pieceType: PieceType,
                                  color: Color,
                                  from: Position,
                                  to: Position,
                                  promotion: Option[Piece]): AlgebraicMove = {
    val captured =
      if (capturedPiece.isDefined) Capture(Some(pieceType), Some((to.col + 96).toChar)) else Capture(None, None)
    val check: Boolean           = IsKingInCheck(color.other, nextBoard)
    val checkmate: Boolean       = IsKingInCheckmate(color.other, nextBoard)
    val promo: Option[PieceType] = if (promotion.isEmpty) None else Some(promotion.get.pieceType)
    AlgebraicSimpleMove(to, pieceType, captured, check, checkmate, Some((from.col + 96).toChar), Some(from.row), promo)
  }
}
