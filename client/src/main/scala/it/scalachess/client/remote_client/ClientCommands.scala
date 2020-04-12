package it.scalachess.client.remote_client

import it.scalachess.util.NetworkMessages.ClientMessage

object ClientCommands {

  /**
   * Commands parsed by the InputParser actor
   */
  trait ClientCommand                extends ClientMessage
  case object Create                 extends ClientCommand
  case class Join(id: String)        extends ClientCommand
  case class InputMove(move: String) extends ClientCommand
  case object Forfeit                extends ClientCommand
  case object Help                   extends ClientCommand
  case object Quit                   extends ClientCommand
  case object Save                   extends ClientCommand

  val helpers: Seq[String] = Seq(
    "/create - Creates a room to play a chess game with another client",
    "/join [id] - Joins an existing room to start a chess game with another client",
    "[move] - Attempts to execute a move on the current chess board. Must be expressed in Short Algebraic Notation (SAN)",
    "/forfeit - Forfeit the current chess game",
    "/help - Prints the list of commands and helpers",
    "/quit - Closes the application",
    "/save - Save the current game moves in PGN format"
  )

}
