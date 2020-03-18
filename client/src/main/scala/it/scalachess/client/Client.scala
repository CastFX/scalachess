package it.scalachess.client

import it.scalachess.core.{ ChessGame }

object Client extends App {
  val game = ChessGame.standard()
  println(game.turn)
  print(game.board.pieces.map { case(p,v) => (p, v.pieceType)})
}
