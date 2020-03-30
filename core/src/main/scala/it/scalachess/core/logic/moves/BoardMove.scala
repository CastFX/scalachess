package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.Piece

sealed trait BoardMove
case class BoardSimpleMove(from: Position, to: Position)                      extends BoardMove
case class BoardPromotion(from: Position, to: Position, piece: Piece)         extends BoardMove
case class BoardCastling(kingPos: Position, rookPos: Position)                extends BoardMove
case class BoardEnPassant(capturePos: Position, from: Position, to: Position) extends BoardMove
