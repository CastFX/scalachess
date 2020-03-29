package it.scalachess.client.remoteClient

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import it.scalachess.client.remoteClient.Client.{ ClientCommand, Create, Forfeit, Join, Move }
import scala.util.matching.Regex

object InputParser {

  val CreateCommand: String  = "/create"
  val ForfeitCommand: String = "/forfeit"
  val JoinCommand: Regex     = """/join (\w)""".r
  val quitCommand: String    = "/quit"

  def apply(parent: ActorRef[ClientCommand]): Behavior[String] = Behaviors.receiveMessage { input =>
    val command: ClientCommand = {
      input match {
        case CreateCommand   => Create
        case ForfeitCommand  => Forfeit
        case JoinCommand(id) => Join(id)
        case _               => Move(input)
      }
    }
    parent ! command
    Behaviors.same
  }
}
