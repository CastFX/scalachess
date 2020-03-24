package it.scalachess.server

import akka.actor.ActorSystem

object Server extends App {
  val system = ActorSystem("server")
}
