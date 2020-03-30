package it.scalachess.server

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.core.Result
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages._

object LobbyManager {

  case class TerminateGame(gameId: String, result: Result, gameManager: ActorRef[GameAction]) extends LobbyMessage

  case class Lobby(gameId: String, players: (ActorRef[ClientMessage], ActorRef[ClientMessage])) {
    def join(player: ActorRef[ClientMessage]): Lobby = Lobby(gameId, (players._1, player))
  }

  type LobbyMap = Map[String, Lobby]

  def apply(): Behavior[LobbyMessage] = Behaviors.setup { context =>
    context.system.receptionist ! Receptionist.Register(NetworkMessages.lobbyServiceKey, context.self)
    discover(Map())
  }

  private def discover(lobbies: LobbyMap): Behavior[LobbyMessage] =
    Behaviors.receive { (context, message) =>
      context.log.debug(message.toString)
      message match {
        case CreateGame(client) => discover(createLobby(client, lobbies))
        case JoinGame(id, client) =>
          val updatedLobbies = joinLobby(id, client, lobbies)
          spawnGameManager(id, updatedLobbies, context)
          discover(updatedLobbies)
        case TerminateGame(id, _, manager) =>
          context stop manager
          discover(removeLobby(id, lobbies))
      }
    }

  private def createLobby(player: ActorRef[ClientMessage], lobbies: LobbyMap): LobbyMap = {
    val gameId = math.abs(lobbies.hashCode).toString
    player ! LobbyId(gameId)
    lobbies + (gameId -> Lobby(gameId, (player, player)))
  }

  private def joinLobby(ofGameId: String, player: ActorRef[ClientMessage], lobbies: LobbyMap): LobbyMap =
    lobbies get ofGameId match {
      case Some(lobby) => lobbies + (ofGameId -> lobby.join(player))
      case _           => lobbies
    }

  private def removeLobby(ofGameId: String, lobbies: LobbyMap): LobbyMap = lobbies - ofGameId

  private def spawnGameManager(ofGameId: String, lobbies: LobbyMap, context: ActorContext[LobbyMessage]): Unit = {
    val _ = context.spawn(GameManager(lobbies(ofGameId), context.self), s"GameManager$ofGameId")
  }
}
