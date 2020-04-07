package it.scalachess.server

import akka.actor.typed.ActorRef
import it.scalachess.util.NetworkMessages.ClientMessage

/**
 * A case class that contains the info about the clients currently connected to a room.
 * Once full, it will be passed to the GameManager to start the chess game
 *
 * @param id  Identifier of the room
 * @param players Tuple of players currently connected, if there is only a player, both fields will contain be that player.
 */
case class Room(id: String, players: (ActorRef[ClientMessage], ActorRef[ClientMessage])) {
  def join(player: ActorRef[ClientMessage]): Room = Room(id, (players._1, player))
  lazy val full: Boolean                          = players._1 != players._2
}

object Room {
  def apply(roomId: String, player: ActorRef[ClientMessage]): Room = Room(roomId, (player, player))
}
