package it.scalachess.server

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.core.{ Black, ChessGame, Color, Ongoing, Result, White, WinByForfeit }
import it.scalachess.util.NetworkErrors.FailedMove
import it.scalachess.util.NetworkMessages._
import scalaz.{ Failure, Success }

import scala.util.Random

/**
 * Companion object used to initialize the Behavior of the GameManager
 */
object GameManager {

  /**
   * Randomizes roles and creates the game. Creates the instance of GameManager
   * @param room contains info about the clients and the gameId
   * @param parent the LobbyManager which will kill this actor after the game ends
   * @return the initial Behavior of this actor
   */
  def apply(room: Room, parent: ActorRef[LobbyMessage]): Behavior[GameAction] =
    Behaviors.setup { context =>
      val players = randomizeRoles(room)
      players.values.foreach { client =>
        context.watchWith(client, ClientDisconnect(client))
      }
      new GameManager(players, room.id, parent).setup()
    }

  private def randomizeRoles(room: Room): Map[Color, ActorRef[ClientMessage]] =
    if (Random.nextInt > 0.5) Map(White -> room.players._1, Black -> room.players._2)
    else Map(White                      -> room.players._2, Black -> room.players._1)
}

/**
 * Class of GameManager which contains all the immutable fields
 * @param players Actors mapped to their color in the chess game
 * @param gameId identifier of this game
 * @param parent LobbyManager which spawned this actor
 */
class GameManager private (players: Map[Color, ActorRef[ClientMessage]],
                           gameId: String,
                           parent: ActorRef[LobbyMessage]) {

  /**
   * Notifies the clients about the game start
   * @return The Behavior of an ongoing game
   */
  def setup(): Behavior[GameAction] =
    Behaviors.setup { context =>
      val game = ChessGame.standard()
      players(game.player) ! GameStart(game.player, game, Move, context.self)
      players(game.player.other) ! GameStart(game.player.other, game, Wait, context.self)
      ongoingGame(game)
    }

  /**
   * Behavior of an ongoing game.
   * Waits for actions from the Clients
   * @param game the up to date ChessGame
   * @return an ongoingGame Behavior with the updated ChessGame
   */
  private def ongoingGame(game: ChessGame): Behavior[GameAction] =
    Behaviors.receive { (context, message) =>
      context.log.debug(message.toString)
      val updated = message match {
        case DoMove(move, player) if playerCanMove(game.player, player) =>
          tryMove(move, game, context.self)

        case x @ (_: ForfeitGame | _: ClientDisconnect) =>
          colorOf(x.asInstanceOf[Sender[ClientMessage]].sender) match {
            case Some(color) =>
              val result        = WinByForfeit(color.other)
              val forfeitedGame = game.end(result)
              players.values.foreach { _ ! GameEnd(result, forfeitedGame) }
              parent ! LobbyManager.TerminateGame(gameId, forfeitedGame, context.self)
            case None => ()
          }
          game
        case _ => game
      }
      ongoingGame(updated)
    }

  /**
   * Tries to apply a move to the ChessGame
   * If successful, updates both Clients, otherwise notifies the moving Client about the failed move
   * @param move Move in string format, in Algebraic Notation
   * @param game The current ChessGame
   * @param self This ActorRef
   * @return an updated ChessGame if successful, the old ChessGame otherwise
   */
  private def tryMove(move: String, game: ChessGame, self: ActorRef[GameAction]): ChessGame =
    game(move) match {
      case Success(updated) =>
        updated.gameStatus match {
          case Ongoing =>
            players(updated.player) ! GameUpdate(updated.player, updated, Move)
            players(updated.player.other) ! GameUpdate(updated.player.other, updated, Wait)
          case result: Result =>
            players.foreach {
              case (color, client) =>
                client ! GameUpdate(color, updated, Wait)
                client ! GameEnd(result, updated)
            }
            parent ! LobbyManager.TerminateGame(gameId, updated, self)
        }
        updated
      case Failure(err) =>
        players(game.player) ! FailedMove(err, move)
        game
    }

  /**
   * Checks if it is the Client's turn to move
   * @param movingColor the color of the moving player
   * @param client the ActorRef of the player trying to move
   * @return true if it is the Client's turn to move, false otherwise
   */
  private def playerCanMove(movingColor: Color, client: ActorRef[ClientMessage]): Boolean =
    colorOf(client).fold(false)(movingColor == _)

  /**
   * Retrieves the Color of a certain ActorRef, if present
   * @param client the ActorRef to identify
   * @return the Color of the ActorRef
   */
  private def colorOf(client: ActorRef[ClientMessage]): Option[Color] =
    (players filter { case (_, player) => player == client }).keys.headOption

}
