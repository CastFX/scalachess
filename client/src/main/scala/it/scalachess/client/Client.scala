package it.scalachess.client

import it.scalachess.core.{Game}


object Client extends App {
  val game = Game("test")
  println(game.identifier)
}