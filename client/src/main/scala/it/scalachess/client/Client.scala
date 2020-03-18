package it.scalachess.client

import it.scalachess.core.ChessGame
import it.scalachess.core.board.Position
import it.scalachess.core.pieces.Piece

object Client extends App {
  val game: ChessGame = ChessGame.standard()
  print(game.board.pieces.map { case (pos: Position, piece: Piece) => (pos, piece.pieceType) }.toString())
}
