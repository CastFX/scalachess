package it.scalachess.client.remoteClient

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remoteClient.Client.{ Create, Forfeit, Join, ParsedMove }
import it.scalachess.client.view.{ CLI, View, ViewFactory }
import it.scalachess.core.colors.Color
import it.scalachess.core._
import it.scalachess.util.NetworkErrors.{ RoomFull, RoomNotFound }
import it.scalachess.util.NetworkMessages._

/**
 * Companion object of the Client Actor, used to setup the children actors
 */
object Client {

  /**
   * Commands parsed by the InputParser actor
   */
  sealed trait ClientCommand                extends ClientMessage
  case object Create                        extends ClientCommand
  final case class Join(id: String)         extends ClientCommand
  final case class ParsedMove(move: String) extends ClientCommand
  case object Forfeit                       extends ClientCommand

  /**
   * Message sent by the Server proxy to notify the established connection to the Server
   */
  case object ConnectedToServer extends ClientMessage

  /**
   * Initializes the Client actor by spawning a ServerProxy and waits for the connection to the server
   * @param serverAddress address of the remote server, with the port e.g:.: 192.168.1.115:25555
   * @return The initial Behavior of the Actor
   */
  def apply(serverAddress: String): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val serverProxy = context.spawn(ServerProxy(serverAddress, context.self), "ServerProxy")
      Behaviors.receiveMessage {
        case ConnectedToServer =>
          context.log.debug("Connected to server")
          new Client(serverProxy).inLobby()
        case _ =>
          Behaviors.same
      }
    }
}

/**
 * Class of the Actor with the initialized ServerProxy
 * @param serverProxy ServerProxy which mediates communications between Client and Server
 */
class Client(serverProxy: ActorRef[ClientMessage]) {

  private val view: View = ViewFactory(CLI)

  /**
   * Waits for user commands to create or join a game
   * @return the Behavior of the client while waiting to create/join a game
   */
  def inLobby(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case cmd @ (Create | Join(_)) =>
      serverProxy ! cmd
      waitForGameStart()
    case _ =>
      Behaviors.same
  }

  /**
   * Clients wait for the server notification that the Game has started
   * @return the Behavior of the client while it waits for the game start
   */
  private def waitForGameStart(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    message match {
      case RoomId(id) =>
        context.log.debug(id)
        Behaviors.same
      case GameStart(color, game, request, _) =>
        view.showBoard(game.board)
        view.showMessage(s"You're $color")
        behaviorForServerRequest(request, color, game)
      case RoomFull(id) =>
        view.showMessage(s"Cannot join room $id, it is full")
        inLobby()
      case RoomNotFound(id) =>
        view.showMessage(s"Cannot join room $id, id not found")
        inLobby()
      case _ =>
        Behaviors.same
    }
  }

  /**
   * Client wait for the opponent to move
   * @param color color of the client in this chess game (White, Black)
   * @param game current state of this chess game
   * @return The Behavior of the client waiting for the opponent to move
   */
  private def waitForOpponentMove(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case GameUpdate(color, game, request, lastMove) =>
      showGameInfo(game, lastMove)
      behaviorForServerRequest(request, color, game)
    case Forfeit =>
      serverProxy ! Forfeit
      inLobby()
    case GameEnd(result, lastMove) =>
      showResult(result, lastMove)
      inLobby()
    case _ =>
      Behaviors.same
  }

  /**
   * It's client turn to move, waits for user input
   * @param color color of the client in this chess game (White, Black)
   * @param game current state of this chess game
   * @return The behavior of the client waiting for user input to send its move
   */
  private def move(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case GameUpdate(_, game, request, lastMove) =>
      showGameInfo(game, lastMove)
      behaviorForServerRequest(request, color, game)
    case message @ (ParsedMove(_) | Forfeit) =>
      val _ = serverProxy ! message
      Behaviors.same
    case GameEnd(result, lastMove) =>
      showResult(result, lastMove)
      inLobby()
    case _ =>
      Behaviors.same
  }

  private def behaviorForServerRequest(request: ServerRequest, color: Color, game: ChessGame): Behavior[ClientMessage] =
    request match {
      case Move =>
        view.showMessage("It's your turn to move")
        move(color, game)
      case Wait =>
        view.showMessage("It's your opponent's turn to move")
        waitForOpponentMove(color, game)
      case _ => inLobby()
    }

  private def showGameInfo(game: ChessGame, lastMove: String): Unit = {
    view.showBoard(game.board)
    if (game.turn > 0) view.showMessage(s"Last move by ${game.player.other} was $lastMove")
  }

  private def showResult(result: Result, lastMove: String): Unit =
    view.showMessage(result match {
      case Win(player)          => s"$player wins with $lastMove"
      case WinByForfeit(player) => s"$player wins by forfeit."
      case Draw                 => "Draw."
    })

}
