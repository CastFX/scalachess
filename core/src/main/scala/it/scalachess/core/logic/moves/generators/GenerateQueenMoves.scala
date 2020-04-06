package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.Queen

private[generators] object GenerateQueenMoves extends GeneratePieceMoves {
  override def apply(color: Color, board: Board, from: Position): List[ValidMove] =
    (GenerateBishopMoves(color, board, from) ++ GenerateRookMoves(color, board, from))
      .map(x => ValidSimpleMove(x.from, x.to, Queen, x.color, x.capture))
}