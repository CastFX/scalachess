package it.scalachess.util

import it.scalachess.util.NetworkMessages.ClientMessage

object NetworkErrors {

  trait ClientError extends ClientMessage

  /**
   * A message sent by the GameManager to a Client who did not send its move in time
   */
  case object MoveTimeout extends ClientError

  /**
   * A message sent by the GameManager to a Client which submitted an illegal move
   * @param error The error caused by the illegal move
   */
  final case class FailedMove(error: String, move: String) extends ClientError

  /**
   * A message sent by the LobbyManager to a Client which tried to join a non-existing room
   * @param id identifier of the room
   */
  final case class RoomNotFound(id: String) extends ClientError

  /**
   * A message sent by the LobbyManager to a Client which tried to join a full room
   * @param id identifier of the room
   */
  final case class RoomFull(id: String) extends ClientError
}
