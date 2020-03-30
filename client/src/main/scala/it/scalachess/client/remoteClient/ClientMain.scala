package it.scalachess.client.remoteClient

import java.net.InetAddress

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory

import scala.io.Source

object ClientMain extends App {

  val privateAddress = InetAddress.getLocalHost.getHostAddress
  val customConf = ConfigFactory
    .parseString(s"""akka.remote.artery.canonicalHostname =  $privateAddress""")
    .withFallback(ConfigFactory.load())
  val serverAddress = if (args.size >= 1) args(0) else "127.0.0.1:25555"
  val client        = ActorSystem(Client(serverAddress), "Client", customConf)
  val inputParser   = client.systemActorOf[String](InputParser(client), "InputReader")

  Source.stdin
    .getLines()
    .takeWhile(_ != InputParser.quitCommand)
    .foreach { inputParser ! _ }

}
