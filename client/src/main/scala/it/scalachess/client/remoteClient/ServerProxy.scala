package it.scalachess.client.remoteClient

import akka.actor.AddressFromURIString
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remoteClient.Client.{ ConnectedToServer, Create, Forfeit, Join, ParsedMove }
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages._

object ServerProxy {
  private final case class ListingResponse(listing: Receptionist.Listing) extends ClientMessage

  val serverSystemName = "LobbyManager"

  def apply(serverAddress: String, parent: ActorRef[ClientMessage]): Behavior[ClientMessage] =
    discoverLobbyManager(serverAddress, parent)

  def discoverLobbyManager(serverAddress: String, parent: ActorRef[ClientMessage]): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val remoteServerAddress       = AddressFromURIString(s"akka://$serverSystemName@$serverAddress/$serverSystemName")
      val receptionistRemoteAddress = context.system.receptionist.path.toStringWithAddress(remoteServerAddress)
      val remoteReceptionist        = context.toClassic.actorSelection(receptionistRemoteAddress)
      val listingResponseAdapter    = context.messageAdapter[Receptionist.Listing](ListingResponse)
      remoteReceptionist ! Receptionist.Find(NetworkMessages.lobbyServiceKey, listingResponseAdapter)

      Behaviors.receiveMessage {
        case ListingResponse(NetworkMessages.lobbyServiceKey.Listing(listings)) =>
          listings.headOption match {
            case Some(lobbyManager) =>
              parent ! ConnectedToServer
              new ServerProxy(lobbyManager, parent).proxyInLobby()
            case None => Behaviors.same
          }
      }
    }
}

class ServerProxy private (lobbyManager: ActorRef[LobbyMessage], parent: ActorRef[ClientMessage]) {

  def proxyInLobby(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    context.log.debug(message.toString)
    message match {
      //To Server
      case Create =>
        lobbyManager ! CreateGame(context.self)
        Behaviors.same
      case Join(id) =>
        lobbyManager ! JoinGame(id, context.self)
        Behaviors.same

      //From Server
      case LobbyId(_) =>
        parent ! message
        Behaviors.same
      case GameStart(_, _, _, manager) =>
        parent ! message
        proxyInGame(manager)
      case _ =>
        Behaviors.same
    }
  }

  def proxyInGame(gameManager: ActorRef[GameAction]): Behavior[ClientMessage] = Behaviors.receive {
    (context, message) =>
      message match {
        //To Server
        case ParsedMove(move) =>
          gameManager ! NetworkMessages.DoMove(move, context.self)
          Behaviors.same
        case Forfeit =>
          gameManager ! NetworkMessages.ForfeitGame(context.self)
          Behaviors.same

        //From Server
        case GameUpdate(_, _, _, _) | FailedMove(_) =>
          parent ! message
          Behaviors.same
        case GameEnd(_) =>
          parent ! message
          proxyInLobby()
        case _ =>
          Behaviors.same
      }
  }
}
