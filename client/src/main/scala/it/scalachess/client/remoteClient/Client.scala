package it.scalachess.client.remoteClient

import akka.NotUsed
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }
import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import akka.stream.scaladsl.StreamConverters
import akka.stream.typed.scaladsl.ActorSink
import it.scalachess.core.ChessGame
import it.scalachess.core.colors.Color
import it.scalachess.util.NetworkMessages.{ ClientMessage, GameEnded, GameStarted }

import scala.concurrent.Future

object Client {

  sealed trait ClientCommand          extends ClientMessage
  case object Create                  extends ClientCommand
  final case class Join(id: String)   extends ClientCommand
  final case class Move(string: Move) extends ClientCommand
  case object Forfeit                 extends ClientCommand
  case object ConnectedToServer       extends ClientMessage

  def apply(): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val serverProxy = context.spawn(ServerProxy(context.self), "ServerProxy")
      val inputReader = context.spawn(InputReader(context.self), "InputReader")
      redirectStdinTo(inputReader, context)

      Behaviors.receiveMessage {
        case ConnectedToServer => new Client(serverProxy).inLobby()
      }
    }

  private def redirectStdinTo(reader: ActorRef[String], context: ActorContext[ClientMessage]): Unit = {
    implicit val system = context.system
    StreamConverters
      .fromInputStream(() => System.in)
      .map(_.utf8String)
      .runWith(ActorSink.actorRef(reader, "done", x => "fail"))
  }
}

class Client(serverProxy: ActorRef[ClientMessage]) {

  import Client._

  def inLobby(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case Create   => serverProxy ! Create; waitingForGameStart()
    case Join(id) => serverProxy ! Join(id); waitingForGameStart()
  }

  def waitingForGameStart(): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case GameStarted(color, game, _) => inGame(color, game)
  }

  def inGame(color: Color, game: ChessGame): Behavior[ClientMessage] = Behaviors.receiveMessage {
    case Move(_) | Forfeit => serverProxy ! _; Behaviors.same
    case GameEnded(result) => println(result); inLobby()
  }

}
