package it.scalachess.client.remoteClient

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remoteClient.Client.{ Create, Forfeit, Join, ParsedMove }
import it.scalachess.client.view.{ CLI, View, ViewFactory }
import it.scalachess.core.{ ChessGame, Draw, Result, Win, WinByForfeit }
import it.scalachess.core.colors.Color
import it.scalachess.util.NetworkMessages.{
  ClientMessage,
  GameEnd,
  GameStart,
  GameUpdate,
  LobbyId,
  Move,
  ServerRequest,
  Wait
}
import org.slf4j.Logger

object Client {

  sealed trait ClientCommand                extends ClientMessage
  case object Create                        extends ClientCommand
  final case class Join(id: String)         extends ClientCommand
  final case class ParsedMove(move: String) extends ClientCommand
  case object Forfeit                       extends ClientCommand
  case object ConnectedToServer             extends ClientMessage

  def apply(serverAddress: String): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val serverProxy = context.spawn(ServerProxy(serverAddress, context.self), "ServerProxy")
      Behaviors.receiveMessage {
        case ConnectedToServer =>
          context.log.debug("Connected to server")
          new Client(serverProxy, context.log).inLobby()
        case _ =>
          Behaviors.same
      }
    }
}

class Client(serverProxy: ActorRef[ClientMessage], logger: Logger) {

  private val view: View = ViewFactory(CLI)

  def inLobby(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case cmd @ (Create | Join(_)) =>
      serverProxy ! cmd
      waitForGameStart()
    case _ =>
      Behaviors.same
  }

  private def waitForGameStart(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    message match {
      case LobbyId(id) =>
        context.log.debug(id)
        Behaviors.same
      case GameStart(color, game, request, _) =>
        view.showBoard(game.board)
        view.showMessage(s"You're $color")
        behaviorForServerRequest(request, color, game)
      case _ =>
        Behaviors.same
    }
  }

  private def waitForOpponentMove(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case GameUpdate(color, game, request, lastMove) =>
      showGameInfo(game, lastMove)
      behaviorForServerRequest(request, color, game)
    case Forfeit =>
      serverProxy ! Forfeit
      inLobby()
    case GameEnd(result) =>
      showResult(result)
      inLobby()
    case _ =>
      Behaviors.same
  }

  private def move(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case GameUpdate(_, game, request, lastMove) =>
      showGameInfo(game, lastMove)
      behaviorForServerRequest(request, color, game)
    case message @ (ParsedMove(_) | Forfeit) =>
      val _ = serverProxy ! message
      Behaviors.same
    case GameEnd(result) =>
      showResult(result)
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

  private def showResult(result: Result): Unit =
    view.showMessage(result match {
      case Win(player)          => s"$player wins."
      case WinByForfeit(player) => s"$player wins by forfeit."
      case Draw                 => "Draw."
    })

}
