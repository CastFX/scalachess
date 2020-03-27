package it.scalachess.server

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import it.scalachess.core.Result
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages.{ ClientMessage, CreateGame, GameAction, JoinGame, LobbyMessage }

object LobbyManager {

  case class TerminateGame(gameId: String,
                           result: Result,
                           gameManager: ActorRef[GameAction],
                           client: ActorRef[ClientMessage])
      extends LobbyMessage

  case class Lobby(gameId: String, players: Seq[ActorRef[ClientMessage]]) {
    def join(player: ActorRef[ClientMessage]): Lobby = Lobby(gameId, players :+ player)
  }

  def apply(): Behavior[LobbyMessage] = Behaviors.setup { context =>
    context.system.receptionist ! Receptionist.Register(NetworkMessages.lobbyServiceKey, context.self)
    discover(Map())
  }

  private def discover(lobbies: Map[String, Lobby]): Behavior[LobbyMessage] =
    Behaviors.receive { (context, message) =>
      message match {
        case CreateGame(client) => discover(createLobby(client, lobbies))
        case JoinGame(id, client) => {
          val updatedLobbies = joinLobby(id, client, lobbies)
          createGameManager(id, lobbies, context)
          discover(updatedLobbies)
        }
        case TerminateGame(id, result, manager, _) => {
          context stop manager
          discover(removeLobby(id, result, lobbies))
        }
      }
    }

  private def createLobby(player: ActorRef[ClientMessage], lobbies: Map[String, Lobby]): Map[String, Lobby] = {
    val gameId = lobbies.hashCode.toString
    lobbies + (gameId -> Lobby(gameId, Seq(player)))
  }

  private def joinLobby(ofGameId: String, player: ActorRef[ClientMessage], lobbies: Map[String, Lobby]) =
    (lobbies get ofGameId) match {
      case Some(lobby) => lobbies + (ofGameId -> lobby.join(player))
      case _           => lobbies
    }

  private def removeLobby(ofGameId: String, result: Result, lobbies: Map[String, Lobby]): Map[String, Lobby] =
    lobbies - ofGameId

  private def createGameManager(ofGameId: String,
                                lobbies: Map[String, Lobby],
                                context: ActorContext[LobbyMessage]): Unit =
    lobbies get ofGameId match {
      case Some(lobby) => context.spawn(GameManager(lobby, context.self), s"GameManager$ofGameId")
    }
}
