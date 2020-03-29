package it.scalachess.util

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.ServiceKey
import it.scalachess.core.colors.Color
import it.scalachess.core.{ ChessGame, Result }

object NetworkMessages {

  trait Sender[T] {
    val sender: ActorRef[T]
  }

  trait ServerMessage
  trait LobbyMessage                                                     extends ServerMessage
  final case class CreateGame(sender: ActorRef[ClientMessage])           extends Sender[ClientMessage] with LobbyMessage
  final case class JoinGame(id: String, sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with LobbyMessage

  sealed trait GameAction                                              extends ServerMessage
  final case class Move(move: String, sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with GameAction
  final case class ForfeitGame(sender: ActorRef[ClientMessage])        extends Sender[ClientMessage] with GameAction

  trait ClientMessage
  final case class LobbyId(id: String) extends ClientMessage
  final case class GameStarted(color: Color, game: ChessGame, sender: ActorRef[GameAction])
      extends Sender[GameAction]
      with ClientMessage

  final case class GameStatus(game: ChessGame) extends ClientMessage
  final case class FailedMove(error: String)   extends ClientMessage

  final case class RequestMove(color: Color, game: ChessGame, sender: ActorRef[GameAction])
      extends Sender[GameAction]
      with ClientMessage

  final case class GameEnded(result: Result) extends ClientMessage
  case object MoveTimeout                    extends ClientMessage

  val lobbyServiceKey: ServiceKey[LobbyMessage] = ServiceKey[LobbyMessage]("lobbyManager")
}
