package it.scalachess.client.remoteClient

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors
import it.scalachess.client.remoteClient.Client.{ ClientCommand, Create, Forfeit, Join, ParsedMove }

import scala.util.matching.Regex

/**
 * Actor which parses String into ClientCommand and notifies its parent
 */
object InputParser {

  val CreateCommand: String  = "/create"
  val ForfeitCommand: String = "/forfeit"
  val JoinCommand: Regex     = """/join ([0-9]+)""".r
  val quitCommand: String    = "/quit"

  def apply(parent: ActorRef[ClientCommand]): Behavior[String] = Behaviors.receiveMessage { input =>
    parent ! inputToCommand(input)
    Behaviors.same
  }

  /**
   * Converts a String into the proper ClientCommand.
   * It assumes that a non-matched input is Move
   * @param input String to be parsed
   * @return A ClientCommand of the input
   */
  def inputToCommand(input: String): ClientCommand =
    input match {
      case CreateCommand   => Create
      case ForfeitCommand  => Forfeit
      case JoinCommand(id) => Join(id)
      case _               => ParsedMove(input)
    }
}
