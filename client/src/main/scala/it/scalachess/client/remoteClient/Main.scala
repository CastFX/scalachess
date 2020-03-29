package it.scalachess.client.remoteClient

import akka.actor.typed.ActorSystem

import scala.io.Source

object Main extends App {
  val system      = ActorSystem(Client(), "Client")
  val inputParser = system.systemActorOf[String](InputParser(system), "InputReader")

  Source.stdin
    .getLines()
    .takeWhile(_ != InputParser.quitCommand)
    .foreach { inputParser ! _ }

}
