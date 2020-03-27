package it.scalachess.client.remoteClient

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.IOResult
import akka.stream.scaladsl.{ Source, StreamConverters }
import akka.util.ByteString
import it.scalachess.client.remoteClient.Client.{ ClientCommand, Move }

import scala.concurrent.Future
import scala.util.matching.Regex

object InputReader {

  val movePattern: Regex           = "[a-z][1]".r //todo move to core
  val specialCommandPattern: Regex = "/([a-z]+) ()*".r

  def apply(parent: ActorRef[ClientCommand]): Behavior[String] = Behaviors.receiveMessage {
    case movePattern(_)          => parent ! Move(_); Behaviors.same
    case specialCommandPattern() => Behaviors.same
  }

  private def parseCommand(command: String): Option[ClientCommand] = None

}
