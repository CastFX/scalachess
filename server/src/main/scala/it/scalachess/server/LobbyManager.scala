package it.scalachess.server

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import it.scalachess.core.ChessGame
import it.scalachess.server.GameManager.{ GameCommand, PlayerRequest }

object LobbyManager {

  sealed trait LobbyCommand
  final case class Initialize(p1: ActorRef[PlayerRequest]) extends LobbyCommand
  final case class Join(p2: ActorRef[PlayerRequest])       extends LobbyCommand
  final case object GameEnded                              extends LobbyCommand

  private val initialLobby = Lobby(Seq(), ChessGame.standard())

  def apply(): Behavior[LobbyCommand] =
    waitingForPlayers(initialLobby)

  private def waitingForPlayers(lobby: Lobby): Behavior[LobbyCommand] =
    Behaviors.receive { (context, message) =>
      message match {
        case Initialize(p1) => waitingForPlayers(Lobby(Seq(p1), lobby.game))
        case Join(p2) => {
          val gameManager = createGameManager(context, lobby join p2)
          gameManager ! GameManager.Start
          waitingForGameEnd(gameManager)
        }
      }
    }

  private def waitingForGameEnd(gameManager: ActorRef[GameCommand]): Behavior[LobbyCommand] =
    Behaviors.receive { (context, message) =>
      message match {
        case GameEnded => {
          context stop gameManager
          waitingForPlayers(initialLobby)
        }
      }
    }

  private def createGameManager(context: ActorContext[LobbyCommand], lobby: Lobby): ActorRef[GameCommand] =
    context.spawn(GameManager(lobby, context.self), "GameManage")
}

final case class Lobby(players: Seq[ActorRef[PlayerRequest]], game: ChessGame) {
  def join(secondPlayer: ActorRef[PlayerRequest]): Lobby =
    Lobby(players :+ secondPlayer, game)
}
