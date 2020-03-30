package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.PieceType

trait GeneratePieceMoves {
  def pieceType: PieceType
  def color: Color
  def board: Board
  def from: Position

  def apply(): Set[ValidMove]
}
