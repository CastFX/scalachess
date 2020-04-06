package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidEnPassant, ValidMove }
import it.scalachess.core.Color
import it.scalachess.core.pieces.Pawn

private[generators] object PawnSpecialMoves extends PieceSpecialMoves {

  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove] =
    enPassant(color, board, from, history)

  private def enPassant(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove] =
    history.lastOption match {
      case None => List()
      case Some(move) =>
        val position = move.validMove.to
        if (isForwardTwoSquares(move.validMove) && isCloseTo(position, from))
          List(ValidEnPassant(from, position.posUp.get, color, position))
        else
          List()
    }
  private def isCloseTo(position: Position, to: Position): Boolean = {
    val posLeft  = position.posLeft
    val posRight = position.posRight
    (posLeft.isDefined && posLeft.get == to) || (posRight.isDefined && posRight.get == to)
  }

  private def isForwardTwoSquares(validMove: ValidMove): Boolean =
    (validMove.from.row - validMove.to.row).abs == 2 && validMove.pieceType == Pawn
}
