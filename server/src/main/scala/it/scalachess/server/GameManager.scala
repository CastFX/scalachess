package it.scalachess.server

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import it.scalachess.core.ChessGame
import it.scalachess.core.board.Position
import it.scalachess.core.colors.{ Black, Color, White }
import it.scalachess.core.pieces.{ King, Piece }
import it.scalachess.server.GameManager.PlayerRequest

import scala.util.Random

object GameManager {

  sealed trait PlayerRequest
  final case class AskMove(game: ChessGame)       extends PlayerRequest
  final case class TellStatus(status: GameStatus) extends PlayerRequest
  final case class SendGame(game: ChessGame)      extends PlayerRequest

  sealed trait GameCommand
  final case object Start                                       extends GameCommand
  final case class Move(move: String)                           extends GameCommand
  final case class RequestGame(sender: ActorRef[PlayerRequest]) extends GameCommand

  //TODO placeholder
  sealed trait GameStatus
  final case object Ongoing              extends GameStatus
  sealed trait Result                    extends GameStatus
  final case class Winner(player: Color) extends Result
  final case object Draw                 extends Result

  final case class End(result: Result)

  def apply(lobby: Lobby, parent: ActorRef[LobbyManager.LobbyCommand]): Behavior[GameCommand] =
    Behaviors.setup { _ =>
      val players = randomizeRoles(lobby)
      new GameManager(players, parent).waitForStart(lobby.game)
    }

  private def randomizeRoles(lobby: Lobby): Map[Color, ActorRef[PlayerRequest]] = {
    val p1Starts = Random.nextInt > 0.5
    (lobby.players.headOption, lobby.players.drop(1).headOption) match {
      case (Some(p1), Some(p2)) =>
        if (p1Starts) Map(White -> p1, Black -> p2)
        else Map(White          -> p2, Black -> p1)
    }
  }

  //TODO placeholder
  private def gameStatus(game: ChessGame): GameStatus =
    if (game.board.pieces.values.toList contains Piece(White, King)) Ongoing
    else Winner(Black)

  private def tryApplyMove(move: String, game: ChessGame): Option[ChessGame] =
    Some(game.move(Position.ofNotation(move).get, Position.ofNotation(move).get))
}

class GameManager private (players: Map[Color, ActorRef[PlayerRequest]], parent: ActorRef[LobbyManager.LobbyCommand]) {

  import GameManager._

  private def waitForStart(game: ChessGame): Behavior[GameCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case Start => {
          askToMove(game)
          ongoingGame(game)
        }
      }
    }

  private def ongoingGame(game: ChessGame): Behavior[GameCommand] =
    Behaviors.receive { (_, message) =>
      message match {
        case Move(move) => {
          tryApplyMove(move, game) match {
            case Some(updated) => {
              askToMove(updated)
              gameStatus(updated) match {
                case Ongoing   => ongoingGame(updated)
                case r: Result => communicateResults(r); Behaviors.same
              }
            }
            case None => askToMove(game); Behaviors.same
          }
        }
        case RequestGame(sender) => sender ! SendGame(game); Behaviors.same
      }
    }

  private def askToMove(game: ChessGame): Unit =
    players get game.player match {
      case Some(p1: ActorRef[PlayerRequest]) => p1 ! AskMove(game)
    }

  private def communicateResults(result: Result): Unit = {
    players.values.foreach { _ ! TellStatus(result) }
    parent ! LobbyManager.GameEnded
  }
}
