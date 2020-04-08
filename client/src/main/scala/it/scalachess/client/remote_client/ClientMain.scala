package it.scalachess.client.remote_client

import akka.actor.typed.ActorSystem
import com.typesafe.config.ConfigFactory
import it.scalachess.util.NetworkUtils

import scala.io.Source

object ClientMain extends App {

  val privateAddress = NetworkUtils.privateIPAddress
  val customConf = ConfigFactory
    .parseString(s"""akka.remote.artery.canonical.hostname =  $privateAddress""")
    .withFallback(ConfigFactory.load())
  val serverAddress = if (args.length >= 1) args(0) else s"$privateAddress:25555"
  val client        = ActorSystem(Client(serverAddress), "Client", customConf)
  val inputParser   = client.systemActorOf[String](InputParser(client), "InputReader")

  //Stdin redirect to an actor, needs to be outside of the actor system because it is a blocking call
  Source.stdin
    .getLines()
    .takeWhile(_ != InputParser.quit)
    .foreach { inputParser ! _ }

  client.terminate()
}
