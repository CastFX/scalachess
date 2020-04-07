package it.scalachess.server

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.core.ChessGame
import it.scalachess.server.LobbyManager.{ RoomMap, TerminateGame }
import it.scalachess.util.NetworkErrors.{ RoomFull, RoomNotFound }
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages._
import org.slf4j.Logger

/**
 * Companion object of LobbyManager to register the actor to the Receptionist
 */
object LobbyManager {
  type RoomMap = Map[String, Room]

  /**
   * A message sent by the GameManager to this actor to notify about the game end result.
   *
   * @param roomId      Identifier of the ended game
   * @param game        Final state of the ended game
   * @param gameManager GameManage in charge of the ended game
   */
  case class TerminateGame(roomId: String, game: ChessGame, gameManager: ActorRef[GameAction]) extends LobbyMessage

  /**
   * Creates the initial Behavior of this actor, with 0 rooms
   *
   * @return the initial Behavior, which is waiting for Clients to create and join rooms.
   */
  def apply(): Behavior[LobbyMessage] = Behaviors.setup { context =>
    context.system.receptionist ! Receptionist.Register(NetworkMessages.lobbyServiceKey, context.self)
    new LobbyManager(context.log).discover(Map())
  }
}

/** A typed actor who lets Client create and join rooms where chess games will be hosted.
 * It also spawns a GameManager for each game ready to start
 * @param logger logs the actions done by the LobbyManager
 */
class LobbyManager(logger: Logger) {

  /**
   * Standard behavior of the LobbyManager
   * It waits for Clients to create or join rooms
   * @param rooms The updated and immutable RoomMap which contains all the rooms mapped to their identifiers
   * @return the Behavior of this Actor
   */
  private def discover(rooms: RoomMap): Behavior[LobbyMessage] =
    Behaviors.receive { (context, message) =>
      message match {
        case CreateRoom(client) =>
          val id      = math.abs(rooms.hashCode).toString
          val newRoom = Room(id, client)
          client ! RoomId(id)
          logger.info(s"Client ${client.hashCode} created room $id")
          discover(rooms + (id -> newRoom))

        case JoinRoom(id, client) =>
          rooms get id match {
            case Some(room) if !room.full =>
              val updatedRoom = room.join(client)
              logger.info(s"Client ${client.hashCode} joined room $id")
              val _ = context.spawn(GameManager(updatedRoom, context.self), s"GameManager$id")
              discover(rooms + (id -> updatedRoom))
            case Some(room) if room.full =>
              client ! RoomFull(id)
              Behaviors.same
            case None =>
              logger.info(s"Client ${client.hashCode} failed to join room $id")
              client ! RoomNotFound(id)
              Behaviors.same
          }

        case TerminateGame(id, _, manager) =>
          context stop manager
          logger.info(s"Removed room $id and stopped GameManager ${manager.hashCode}")
          discover(rooms - id)

        case _ =>
          Behaviors.same
      }
    }
}
