package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidEnPassant, ValidMove }
import it.scalachess.core.Color
import it.scalachess.core.pieces.Pawn

private[generators] object GeneratePawnSpecialMoves extends GeneratePieceSpecialMoves {

  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove] =
    generateEnPassant(color, board, from, history)

  private def generateEnPassant(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove] =
    history.lastOption match {
      case None => List()
      case Some(move) =>
        if ((move.validMove.from.row - move.validMove.to.row != 2) && move.validMove.pieceType != Pawn) {
          List()
        } else {
          val posLeft  = move.validMove.to.posLeft
          val posRight = move.validMove.to.posRight
          val posUp    = move.validMove.to.posUp
          if (posLeft.isDefined && posLeft.get == from || posRight.isDefined && posRight.get == from) {
            List(ValidEnPassant(from, posUp.get, color, move.validMove.to))
          } else {
            List()
          }
        }
    }
}
