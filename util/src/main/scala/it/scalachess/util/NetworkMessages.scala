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

  sealed trait GameAction                                                extends ServerMessage
  final case class DoMove(move: String, sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with GameAction
  final case class ForfeitGame(sender: ActorRef[ClientMessage])          extends Sender[ClientMessage] with GameAction

  trait ClientMessage
  final case class LobbyId(id: String) extends ClientMessage

  sealed trait ServerRequest
  case object Move extends ServerRequest
  case object Wait extends ServerRequest

  final case class GameStart(color: Color, game: ChessGame, request: ServerRequest, sender: ActorRef[GameAction])
      extends Sender[GameAction]
      with ClientMessage
  final case class GameUpdate(color: Color, game: ChessGame, request: ServerRequest, lastMove: String)
      extends ClientMessage
  final case class GameEnd(result: Result) extends ClientMessage

  case object MoveTimeout                    extends ClientMessage
  final case class FailedMove(error: String) extends ClientMessage

  val lobbyServiceKey: ServiceKey[LobbyMessage] = ServiceKey[LobbyMessage]("lobbyManager")
}
