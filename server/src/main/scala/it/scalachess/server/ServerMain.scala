package it.scalachess.server

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory
import it.scalachess.util.NetworkMessages.LobbyMessage
import it.scalachess.util.NetworkUtils

object ServerMain extends App {

  val privateAddress = NetworkUtils.privateIPAddress
  val port           = if (args.length >= 1) args(0) else "0"
  val customConf = ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.hostname =  $privateAddress
         |akka.remote.artery.canonical.port = $port
         |""".stripMargin)
    .withFallback(ConfigFactory.load())
  val system: ActorSystem[LobbyMessage] = ActorSystem(LobbyManager(), "LobbyManager", customConf)
}
