package it.scalachess.client.remoteClient

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remoteClient.Client.{ Create, Forfeit, Join, Move }
import it.scalachess.core.ChessGame
import it.scalachess.core.colors.Color
import it.scalachess.util.NetworkMessages.{ ClientMessage, GameEnded, GameStarted, LobbyId }

object Client {

  sealed trait ClientCommand          extends ClientMessage
  case object Create                  extends ClientCommand
  final case class Join(id: String)   extends ClientCommand
  final case class Move(move: String) extends ClientCommand
  case object Forfeit                 extends ClientCommand
  case object ConnectedToServer       extends ClientMessage

  def apply(): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val serverProxy = context.spawn(ServerProxy(context.self), "ServerProxy")
      Behaviors.receiveMessage {
        case ConnectedToServer => new Client(serverProxy).inLobby()
        case _                 => Behaviors.same
      }
    }
}

class Client(serverProxy: ActorRef[ClientMessage]) {

  def inLobby(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case Create   => serverProxy ! Create; waitingForGameStart()
    case Join(id) => serverProxy ! Join(id); waitingForGameStart()
    case _        => Behaviors.same
  }

  def waitingForGameStart(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    message match {
      case LobbyId(id)                 => context.log.debug(id); Behaviors.same
      case GameStarted(color, game, _) => inGame(color, game)
      case _                           => Behaviors.same
    }
  }

  def inGame(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case Move(_) | Forfeit => val _ = serverProxy ! _; Behaviors.same
    case GameEnded(result) => println(result); inLobby()
    case _                 => Behaviors.same
  }

}
