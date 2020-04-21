package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidEnPassant, ValidMove }
import it.scalachess.core.Color
import it.scalachess.core.pieces.Pawn

private[generators] object EnPassant extends PieceSpecialMoves {

  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): Seq[ValidMove] =
    enPassant(color, board, from, history).toSeq

  private def enPassant(color: Color, board: Board, from: Position, history: Seq[FullMove]): Option[ValidMove] =
    history.lastOption match {
      case Some(FullMove(move, _, _, _)) if isForwardTwoSquares(move) && isCloseTo(move.to, from) =>
        Some(ValidEnPassant(from, move.to.posUp.get, color, move.to))
      case _ => None
    }
  private def isCloseTo(position: Position, to: Position): Boolean =
    position.posLeft.fold(false)(_.equals(to)) || position.posRight.fold(false)(_.equals(to))

  private def isForwardTwoSquares(validMove: ValidMove): Boolean =
    (validMove.from.row - validMove.to.row).abs == 2 && validMove.pieceType == Pawn
}
