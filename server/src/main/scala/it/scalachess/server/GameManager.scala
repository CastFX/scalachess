package it.scalachess.server

import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.core.colors.{ Black, Color, White }
import it.scalachess.core.{ ChessGame, Ongoing, Result, WinByForfeit }
import it.scalachess.server.LobbyManager.Lobby
import it.scalachess.util.NetworkMessages._
import scalaz.{ Failure, Success }

import scala.util.Random

object GameManager {

  def apply(lobby: Lobby, parent: ActorRef[LobbyMessage]): Behavior[GameAction] =
    Behaviors.setup { context =>
      val players = randomizeRoles(lobby)
      val game    = ChessGame.standard()
      new GameManager(players, lobby.gameId, context, parent).setup(game)
    }

  private def randomizeRoles(lobby: Lobby): Map[Color, ActorRef[ClientMessage]] =
    if (Random.nextInt > 0.5) Map(White -> lobby.players._1, Black -> lobby.players._2)
    else Map(White                      -> lobby.players._2, Black -> lobby.players._1)
}

class GameManager private (players: Map[Color, ActorRef[ClientMessage]],
                           gameId: String,
                           context: ActorContext[GameAction],
                           server: ActorRef[LobbyMessage]) {

  def setup(game: ChessGame): Behavior[GameAction] =
    Behaviors.setup { _ =>
      players(game.player) ! GameStart(game.player, game, Move, context.self)
      players(game.player.other) ! GameStart(game.player.other, game, Wait, context.self)
      ongoingGame(game)
    }

  private def ongoingGame(game: ChessGame): Behavior[GameAction] =
    Behaviors.receive { (context, message) =>
      context.log.debug(message.toString)
      val updated = message match {
        case DoMove(move, player) if playerCanMove(game, player) =>
          tryMove(move, game)

        case ForfeitGame(client) =>
          colorOf(client) match {
            case Some(color) =>
              val forfeit = WinByForfeit(color.other)
              players.values.foreach { _ ! GameEnd(forfeit) }
              server ! LobbyManager.TerminateGame(gameId, forfeit, context.self)
            case _ =>
              client ! FailedMove("Cannot forfeit")
          }
          game

        case _ => game
      }
      ongoingGame(updated)
    }

  private def tryMove(move: String, game: ChessGame): ChessGame =
    game(move) match {
      case Success(updated) =>
        updated.gameStatus match {
          case Ongoing =>
            players(updated.player) ! GameUpdate(updated.player, updated, Move, move)
            players(updated.player.other) ! GameUpdate(updated.player.other, updated, Wait, move)
          case result: Result =>
            players.values.foreach { _ ! GameEnd(result) }
            server ! LobbyManager.TerminateGame(gameId, result, context.self)
        }
        updated
      case Failure(err) =>
        players(game.player) ! FailedMove(err)
        game
    }

  private def playerCanMove(game: ChessGame, player: ActorRef[ClientMessage]): Boolean =
    colorOf(player).fold(false)(game.player == _)

  private def colorOf(client: ActorRef[ClientMessage]): Option[Color] = //Todo test actorRef equality
    (players filter { case (_, player) => player == client }).keys.headOption

}
