package it.scalachess.server

import akka.actor.typed.ActorSystem
import it.scalachess.util.NetworkMessages.LobbyMessage

object Main extends App {
  val system: ActorSystem[LobbyMessage] = ActorSystem(LobbyManager(), "LobbyManager")
}
