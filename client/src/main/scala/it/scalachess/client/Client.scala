package it.scalachess.client

import it.scalachess.core.{ChessGame}


object Client extends App {
  val game = ChessGame()
  println(game.turn)
}