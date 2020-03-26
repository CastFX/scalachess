package it.scalachess.server

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory

object Server extends App {

  val port = 25555

  val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      """).withFallback(ConfigFactory.load())

  val system: ActorSystem[LobbyManager.LobbyCommand] = ActorSystem(LobbyManager(), "", config)
}
