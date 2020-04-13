package it.scalachess.client.remote_client

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remote_client.Client.commandNotFound
import it.scalachess.client.remote_client.ClientCommands._
import it.scalachess.client.view.ViewCommands.{ ShowBoard, ShowMessage, ShowResult, ViewMessage }
import it.scalachess.client.view.{ CLI, ViewType, Viewer }
import it.scalachess.core._
import it.scalachess.core.parser.GameSaverParser
import it.scalachess.util.NetworkErrors.{ FailedMove, MatchNotFound, RoomFull, RoomNotFound }
import it.scalachess.util.NetworkMessages._

/**
 * Companion object of the Client Actor, used to setup the children actors
 */
object Client {

  /**
   * Message sent by the Server proxy to notify the established connection to the Server
   */
  case object ConnectedToServer extends ClientMessage

  private val viewType: ViewType      = CLI
  private val commandNotFound: String = "Command Not Found"

  /**
   * Initializes the Client actor by spawning a ServerProxy and waits for the connection to the server
   * @param serverAddress address of the remote server, with the port e.g:.: 192.168.1.115:25555
   * @return The initial Behavior of the Actor
   */
  def apply(serverAddress: String): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val view = context.spawn(Viewer(context.self, viewType), "View")
      view ! ShowMessage(s"Connecting to Server $serverAddress")
      showHelp(view)
      val serverProxy = context.spawn(ServerProxy(serverAddress, context.self), "ServerProxy")
      Behaviors.receiveMessage {
        case ConnectedToServer =>
          view ! ShowMessage("Connected to server")
          new Client(serverProxy, view).inLobby()
        case Help =>
          showHelp(view)
          Behaviors.same
        case CommandNotFound =>
          view ! ShowMessage(commandNotFound)
          Behaviors.same
        case _ =>
          Behaviors.same
      }
    }

  private def showHelp(view: ActorRef[ViewMessage]): Unit = ClientCommands.helpers.foreach(view ! ShowMessage(_))
}

/**
 * Class of the Actor with the initialized ServerProxy
 * @param serverProxy ServerProxy which mediates communications between Client and Server
 */
class Client private (serverProxy: ActorRef[ClientMessage], view: ActorRef[ViewMessage]) {

  /**
   * Waits for user commands to create or join a game
   * @return the Behavior of the client while waiting to create/join a game
   */
  def inLobby(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case cmd @ (Create | Join(_) | Join) =>
      serverProxy ! cmd
      waitForGameStart()
    case Help =>
      Client.showHelp(view)
      Behaviors.same
    case CommandNotFound =>
      view ! ShowMessage(commandNotFound)
      Behaviors.same
    case _ =>
      Behaviors.same
  }

  /**
   * Clients wait for the server notification that the Game has started
   * @return the Behavior of the client while it waits for the game start
   */
  private def waitForGameStart(): Behavior[ClientMessage] = Behaviors.receive { (_, message) =>
    message match {
      case RoomId(id) =>
        view ! ShowMessage(id)
        Behaviors.same
      case GameStart(color, game, request, _) =>
        view ! ShowBoard(game.board)
        view ! ShowMessage(s"You're $color")
        behaviorForServerRequest(request, color, game)
      case RoomFull(id) =>
        view ! ShowMessage(s"Cannot join room $id, it is full")
        inLobby()
      case RoomNotFound(id) =>
        view ! ShowMessage(s"Cannot join room $id, id not found")
        inLobby()
      case MatchNotFound =>
        view ! ShowMessage(s"There's not a match to join")
        inLobby()
      case Help =>
        Client.showHelp(view)
        Behaviors.same
      case CommandNotFound =>
        view ! ShowMessage(commandNotFound)
        Behaviors.same
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
    case GameUpdate(color, game, request) =>
      view ! ShowBoard(game.board)
      behaviorForServerRequest(request, color, game)
    case Forfeit =>
      serverProxy ! Forfeit
      inLobby()
    case GameEnd(result, game) =>
      view ! ShowBoard(game.board)
      view ! ShowResult(result)
      inLobby()
    case FailedMove(error, move) =>
      view ! ShowMessage(s"Move: $move failed because of error: $error")
      Behaviors.same
    case Help =>
      Client.showHelp(view)
      Behaviors.same
    case Save =>
      view ! ShowMessage(GameSaverParser.parseAndConvert(game.moveHistory))
      Behaviors.same
    case CommandNotFound =>
      view ! ShowMessage(commandNotFound)
      Behaviors.same
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
    case GameUpdate(_, game, request) =>
      view ! ShowBoard(game.board)
      behaviorForServerRequest(request, color, game)
    case message @ (InputMove(_) | Forfeit) =>
      val _ = serverProxy ! message
      Behaviors.same
    case GameEnd(result, game) =>
      view ! ShowBoard(game.board)
      view ! ShowResult(result)
      inLobby()
    case FailedMove(error, move) =>
      view ! ShowMessage(s"Move: $move failed because of error: $error")
      Behaviors.same
    case Help =>
      Client.showHelp(view)
      Behaviors.same
    case CommandNotFound =>
      view ! ShowMessage(commandNotFound)
      Behaviors.same
    case Save =>
      view ! ShowMessage(GameSaverParser.parseAndConvert(game.moveHistory))
      Behaviors.same
    case _ =>
      Behaviors.same
  }

  private def behaviorForServerRequest(request: ServerRequest, color: Color, game: ChessGame): Behavior[ClientMessage] =
    request match {
      case Move =>
        view ! ShowMessage("It's your turn to move")
        move(color, game)
      case Wait =>
        view ! ShowMessage("It's your opponent's turn to move")
        waitForOpponentMove(color, game)
      case _ => inLobby()
    }
}
