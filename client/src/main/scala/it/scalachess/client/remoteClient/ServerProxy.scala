package it.scalachess.client.remoteClient

import akka.actor.typed.receptionist.Receptionist
import akka.actor.{ActorSelection, AddressFromURIString}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Behavior}
import it.scalachess.client.remoteClient.Client.{ClientCommand, ConnectedToServer, Create, Join}
import it.scalachess.core.colors.Color
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages.{ClientMessage, CreateGame, FailedMove, GameAction, GameEnded, GameStarted, JoinGame, LobbyMessage, RequestMove}

object ServerProxy {
  private final case class ListingResponse(listing: Receptionist.Listing) extends ClientMessage

  val address: String  = "127.0.0.1"
  val port: Int        = 25555
  val serverSystemName = "lobbyManager"

  def apply(parent: ActorRef[ClientMessage]): Behavior[ClientMessage] = discoverLobbyManager(parent)

  def discoverLobbyManager(parent: ActorRef[ClientMessage]): Behavior[ClientMessage] = Behaviors.setup { context =>
    val remoteServerAddress       = AddressFromURIString(s"akka://$address:$port/$serverSystemName")
    val receptionistRemoteAddress = context.system.receptionist.path.toStringWithAddress(remoteServerAddress)
    val remoteReceptionist        = context.toClassic.actorSelection(receptionistRemoteAddress)
    val listingResponseAdapter    = context.messageAdapter[Receptionist.Listing](ListingResponse)
    remoteReceptionist ! Receptionist.Find(NetworkMessages.lobbyServiceKey, listingResponseAdapter)

    Behaviors.receiveMessage {
      case ListingResponse(NetworkMessages.lobbyServiceKey.Listing(listings)) =>
        listings.headOption match {
          case Some(lobbyManager) => parent ! ConnectedToServer; new ServerProxy(lobbyManager, parent).proxy()
        }
    }
  }
}

class ServerProxy private (lobbyManager: ActorRef[LobbyMessage], parent: ActorRef[ClientMessage]) {

  def proxy(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    message match {
      //lobby
      case Create   => lobbyManager ! CreateGame(context.self)
      case Join(id) => lobbyManager ! JoinGame(id, context.self)

      case GameStarted(_, _, _)      => _ = (parent ! message)
      case RequestMove(_, game, _) => println(game.board.pieces); _ = (parent ! message)//Todo testing, remove
      case FailedMove(error)                => println(error); _ = (parent ! message)
      case GameEnded(result)                => println(result); _ = (parent ! message)

    }
    Behaviors.same
  }
}
