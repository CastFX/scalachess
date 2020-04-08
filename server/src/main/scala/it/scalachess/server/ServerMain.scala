package it.scalachess.server

import java.net.InetAddress

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory
import it.scalachess.util.NetworkMessages.LobbyMessage

object ServerMain extends App {

  val privateAddress = InetAddress.getLocalHost.getHostAddress
  val customConf = ConfigFactory
    .parseString(s"""akka.remote.artery.canonicalHostname =  $privateAddress""")
    .withFallback(ConfigFactory.load())
  val system: ActorSystem[LobbyMessage] = ActorSystem(LobbyManager(), "LobbyManager", customConf)
}
