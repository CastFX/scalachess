package it.scalachess.util

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.ServiceKey
import it.scalachess.core.logic.Result
import it.scalachess.core.{ ChessGame, Color }

/**
 * A collection of messages passed between Client and Server
 */
object NetworkMessages {

  /**
   * A trait to add the sender to the Message
   * @tparam T The type of messages which the sender can process
   */
  trait Sender[T] {
    val sender: ActorRef[T]
  }

  /**
   * A trait for the messages sent for the Server
   */
  trait ServerMessage

  /**
   * A trait for the messages sent to the LobbyManager, in the Server
   */
  trait LobbyMessage extends ServerMessage

  /**
   * A message by a Client sent to create a new room of chess between two players. Sent to the LobbyManager
   * @param sender The client who wants to create the room
   */
  final case class CreateRoom(sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with LobbyMessage

  /**
   * A message by a Client sent to join an existing room. Sent to the LobbyManager
   * @param id The room identifier
   * @param sender The client who wants to join the room
   */
  final case class JoinRoom(id: String, sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with LobbyMessage

  /**
   * A message by a Client sent to join an existing room. Sent to the LobbyManager
   * @param sender The client who wants to join the room
   */
  final case class JoinMatch(sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with LobbyMessage

  /**
   * A trait for the messages sent to the GameManager, in the Server. They represent the possible actions of a chess game
   */
  sealed trait GameAction extends ServerMessage

  /**
   * A message sent to the GameManager to execute a move on the board. Depending on the board, the move may or may not be successfully applied
   * @param move The requested move in chess. The syntax is the Algebraic Notation
   * @param sender The client who wants to move
   */
  final case class DoMove(move: String, sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with GameAction

  /**
   * A message sent to the GameManager to declare forfeit
   * @param sender The client who wants to forfeit the game
   */
  final case class ForfeitGame(sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with GameAction

  final case class ClientDisconnect(sender: ActorRef[ClientMessage]) extends Sender[ClientMessage] with GameAction

  /**
   * A trait for the messages sent to the Client
   */
  trait ClientMessage

  /**
   * A message sent by the LobbyManager to let the Client that the room has been created and it has a certain identifier
   * @param id the room identifier
   */
  final case class RoomId(id: String) extends ClientMessage

  /**
   * A trait for the actions that requested by the Server to the Clients
   */
  sealed trait ServerRequest

  /**
   * The Server asks the Client to send a Move
   */
  case object Move extends ServerRequest

  /**
   * The Server asks the Client to wait for the opponent's Move
   */
  case object Wait extends ServerRequest

  /**
   * A message sent by the GameManager to notify the Clients about the start of the chess game.
   * It also provides info on who the Clients should send game messages to, their roles and what to do.
   * @param color The color assigned to the Client, usually White or Black
   * @param game The initial ChessGame
   * @param request The action requested to the Client
   * @param sender The GameManager which will partake in the communications with the Client
   */
  final case class GameStart(color: Color, game: ChessGame, request: ServerRequest, sender: ActorRef[GameAction])
      extends Sender[GameAction]
      with ClientMessage

  /**
   * A message sent by the GameManager to provide updates about the ongoing chess game.
   * Sent from the GameManager to the Clients.
   * @param color The color of the client.
   * @param game The current state of the chess game
   * @param request The action requested to the Client
   */
  final case class GameUpdate(color: Color, game: ChessGame, request: ServerRequest) extends ClientMessage

  /**
   * A message sent by the GameManager to notify the Clients about the end of the chess game.
   * @param result The result of the chess game
   * @param chessGame The final state of the chess game
   */
  final case class GameEnd(result: Result, chessGame: ChessGame) extends ClientMessage

  /**
   * The service key used by the Server to register its LobbyManager to the Receptionist of its ActorSystem
   */
  val lobbyServiceKey: ServiceKey[LobbyMessage] = ServiceKey[LobbyMessage]("lobbyManager")
}
