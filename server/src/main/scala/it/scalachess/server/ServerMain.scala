package it.scalachess.server

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory
import it.scalachess.util.NetworkMessages.LobbyMessage
import it.scalachess.util.NetworkUtils

object ServerMain extends App {

  val privateAddress = NetworkUtils.privateIPAddress
  val customConf = ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.hostname =  $privateAddress""")
    .withFallback(ConfigFactory.load())
  val system: ActorSystem[LobbyMessage] = ActorSystem(LobbyManager(), "LobbyManager", customConf)
}
