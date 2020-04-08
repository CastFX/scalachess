package it.scalachess.client.remote_client

import akka.actor.AddressFromURIString
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.remote_client.Client.ConnectedToServer
import it.scalachess.client.remote_client.ClientCommands.{ Create, Forfeit, InputMove, Join }
import it.scalachess.util.NetworkErrors.ClientError
import it.scalachess.util.NetworkMessages
import it.scalachess.util.NetworkMessages._

/**
 * Companion Object of the Actor in charge of receiving and sending messages to the Server
 * Contains all the logic behind the connection to the remote Actors of the Server
 */
object ServerProxy {
  private final case class AdaptedListing(listing: Receptionist.Listing) extends ClientMessage

  val serverSystemName = "LobbyManager"

  /**
   * Sets up the connection with the server's LobbyManager
   * First it asks the server's Receptionist to retrieve the ActorRef of the LobbyManager.
   * As soon as a AdaptedListing message has been received the connection to the server is established.
   * @param serverAddress remote address of the server, including the port
   * @param parent ActorRef of the Client parent. ServerProxy will forward messages from the Server to it
   * @return the Behavior of this Actor
   */
  def apply(serverAddress: String, parent: ActorRef[ClientMessage]): Behavior[ClientMessage] =
    discoverLobbyManager(serverAddress, parent)

  private def discoverLobbyManager(serverAddress: String, parent: ActorRef[ClientMessage]): Behavior[ClientMessage] =
    Behaviors.setup { context =>
      val remoteServerAddress       = AddressFromURIString(s"akka://$serverSystemName@$serverAddress/$serverSystemName")
      val receptionistRemoteAddress = context.system.receptionist.path.toStringWithAddress(remoteServerAddress)
      val remoteReceptionist        = context.toClassic.actorSelection(receptionistRemoteAddress)
      val listingResponseAdapter    = context.messageAdapter[Receptionist.Listing](AdaptedListing)
      remoteReceptionist ! Receptionist.Find(NetworkMessages.lobbyServiceKey, listingResponseAdapter)

      Behaviors.receiveMessage {
        case AdaptedListing(NetworkMessages.lobbyServiceKey.Listing(listings)) =>
          listings.headOption match {
            case Some(lobbyManager) =>
              parent ! ConnectedToServer
              new ServerProxy(lobbyManager, parent).proxyInLobby()
            case None => Behaviors.same
          }
      }
    }
}

/**
 * Class of ServerProxy after establishing a connection with the Server
 */
class ServerProxy(lobbyManager: ActorRef[LobbyMessage], parent: ActorRef[ClientMessage]) {

  /**
   * Behavior of the Actor when the Client is currently in the lobby
   * Forwards lobby commands to the LobbyManager
   * Forwards server messages to the Client
   * @return Behavior of the Actor when the Client is currently in the lobby
   */
  def proxyInLobby(): Behavior[ClientMessage] = Behaviors.receive { (context, message) =>
    context.log.debug(message.toString)
    message match {
      //To Server
      case Create =>
        lobbyManager ! CreateRoom(context.self)
        Behaviors.same
      case Join(id) =>
        lobbyManager ! JoinRoom(id, context.self)
        Behaviors.same

      //From Server
      case RoomId(_) =>
        parent ! message
        Behaviors.same
      case GameStart(_, _, _, manager) =>
        parent ! message
        proxyInGame(manager)
      case error: ClientError =>
        parent ! error
        Behaviors.same
      case _ =>
        Behaviors.same
    }
  }

  /**
   * Behavior of the Actor when the Client is currently in a chess game
   * Forwards game actions to the GameManager, in the server
   * Forwards server messages from the GameManager to the Client
   * @param gameManager ActorRef of the GameManager in charge of handling the flow of the game, in the server
   * @return Behavior of the Actor when the Client is currently in a chess game
   */
  def proxyInGame(gameManager: ActorRef[GameAction]): Behavior[ClientMessage] = Behaviors.receive {
    (context, message) =>
      message match {
        //To Server
        case InputMove(move) =>
          gameManager ! NetworkMessages.DoMove(move, context.self)
          Behaviors.same
        case Forfeit =>
          gameManager ! NetworkMessages.ForfeitGame(context.self)
          Behaviors.same

        //From Server
        case GameUpdate(_, _, _) =>
          parent ! message
          Behaviors.same
        case GameEnd(_, _) =>
          parent ! message
          proxyInLobby()
        case error: ClientError =>
          parent ! error
          Behaviors.same
        case _ =>
          Behaviors.same
      }
  }
}
