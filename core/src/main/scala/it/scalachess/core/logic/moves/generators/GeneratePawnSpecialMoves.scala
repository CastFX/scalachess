package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType
import it.scalachess.core.{ Black, Color, White }

private[generators] object GeneratePawnSpecialMoves extends GeneratePieceSpecialMoves {

  override def apply(pieceType: PieceType,
                     color: Color,
                     board: Board,
                     from: Position,
                     history: List[ValidMove]): List[ValidMove] = {
    color match {
      case White => generatePawnSimpleMoves()
      case Black => generatePawnSimpleMoves()
    }
    List()
  }
  private def generatePawnSimpleMoves(): Unit = ???
}
