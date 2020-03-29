package it.scalachess.client.remoteClient

import akka.actor.AddressFromURIString
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remoteClient.Client.{ ConnectedToServer, Create, Join }
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages._

object ServerProxy {
  private final case class ListingResponse(listing: Receptionist.Listing) extends ClientMessage

  val address: String  = "127.0.0.1"
  val port: Int        = 25555
  val serverSystemName = "LobbyManager"

  def apply(parent: ActorRef[ClientMessage]): Behavior[ClientMessage] = discoverLobbyManager(parent)

  def discoverLobbyManager(parent: ActorRef[ClientMessage]): Behavior[ClientMessage] = Behaviors.setup { context =>
    val remoteServerAddress = AddressFromURIString(s"akka://$serverSystemName@$address:$port/$serverSystemName")
//    val remoteReceptionistAddress =
    s"akka://$serverSystemName@$address:$port/system/clusterReceptionist"
    val receptionistRemoteAddress = context.system.receptionist.path.toStringWithAddress(remoteServerAddress)
    println(receptionistRemoteAddress)
    val remoteReceptionist = context.toClassic.actorSelection(receptionistRemoteAddress)
    println(remoteReceptionist.anchorPath.address.toString)
    val listingResponseAdapter = context.messageAdapter[Receptionist.Listing](ListingResponse)
    remoteReceptionist ! Receptionist.Find(NetworkMessages.lobbyServiceKey, listingResponseAdapter)

    Behaviors.receiveMessage {
      case ListingResponse(NetworkMessages.lobbyServiceKey.Listing(listings)) =>
        listings.headOption match {
          case Some(lobbyManager) =>
            parent ! ConnectedToServer
            new ServerProxy(lobbyManager, parent).proxy()
          case None => Behaviors.same
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

      case LobbyId(_)              => parent ! message
      case GameStarted(_, _, _)    => parent ! message
      case RequestMove(_, game, _) => println(game.board.pieces); parent ! message //Todo testing, remove println
      case FailedMove(error)       => println(error); parent ! message
      case GameEnded(result)       => println(result); parent ! message

    }
    Behaviors.same
  }
}
