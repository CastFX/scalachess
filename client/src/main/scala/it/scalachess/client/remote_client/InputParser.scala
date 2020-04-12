package it.scalachess.client.remote_client

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remote_client.ClientCommands.{
  ClientCommand,
  Create,
  Forfeit,
  Help,
  InputMove,
  Join,
  Quit,
  Save
}

import scala.util.matching.Regex

/**
 * Actor which parses String into ClientCommand and notifies its parent
 */
object InputParser {

  val create: String  = "/create"
  val join: Regex     = "/join ([a-zA-Z0-9]+)".r
  val forfeit: String = "/forfeit"
  val help: String    = "/help"
  val quit: String    = "/quit"
  val save: String    = "/save"
  val play: String    = "/play"

  def apply(parent: ActorRef[ClientCommand]): Behavior[String] = Behaviors.receiveMessage { input =>
    parent ! inputToCommand(input)
    Behaviors.same
  }

  /**
   * Converts a String into the proper ClientCommand.
   * It assumes that a non-matched input is an InputMove
   * @param input String to be parsed
   * @return A ClientCommand of the input
   */
  def inputToCommand(input: String): ClientCommand =
    input match {
      case `create`  => Create
      case join(id)  => Join(id)
      case `forfeit` => Forfeit
      case `help`    => Help
      case `quit`    => Quit
      case `save`    => Save
      case `play`    => Join
      case _         => InputMove(input)
    }
}
