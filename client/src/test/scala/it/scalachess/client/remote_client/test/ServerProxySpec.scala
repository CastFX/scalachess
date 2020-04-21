package it.scalachess.client.remote_client.test

import akka.actor.testkit.typed.scaladsl.{ ActorTestKit, TestProbe }
import it.scalachess.client.remote_client.ClientCommands.{ Create, Forfeit, InputMove, Join }
import it.scalachess.client.remote_client.ServerProxy
import it.scalachess.core.logic.Win
import it.scalachess.core.{ ChessGame, Color, White }
import it.scalachess.util.NetworkErrors.{ FailedMove, RoomFull, RoomNotFound }
import it.scalachess.util.NetworkMessages._
import org.scalatest._

class ServerProxySpec extends FlatSpec with BeforeAndAfterAll with Matchers with OptionValues with Inspectors {
  val testKit = ActorTestKit()
  import testKit._
  override def afterAll(): Unit                  = testKit.shutdownTestKit()
  val lobbyManagerProbe: TestProbe[LobbyMessage] = TestProbe[LobbyMessage]()
  val gameManagerProbe: TestProbe[GameAction]    = TestProbe[GameAction]()
  val clientProbe: TestProbe[ClientMessage]      = TestProbe[ClientMessage]()
  val actorServerProxy: ServerProxy              = new ServerProxy(lobbyManagerProbe.ref, clientProbe.ref)
  val roomId: String                             = "123"
  val move: String                               = "f3"
  val color: Color                               = White
  val game: ChessGame                            = ChessGame.standard()
  val request: ServerRequest                     = Wait
  "ServerProxy in lobby" should "forward messages from the LobbyManager(server) to the Client" in {
    val serverProxy = spawn(actorServerProxy.proxyInLobby())
    serverProxy ! RoomId(roomId)
    clientProbe expectMessage RoomId(roomId)

    serverProxy ! GameStart(color, game, request, gameManagerProbe.ref)
    clientProbe.expectMessageType[GameStart]
  }

  it should "forward errors from the LobbyManager(server) to the Client" in {
    val serverProxy = spawn(actorServerProxy.proxyInLobby())
    serverProxy ! RoomNotFound(roomId)
    clientProbe expectMessage RoomNotFound(roomId)

    serverProxy ! RoomFull(roomId)
    clientProbe expectMessage RoomFull(roomId)
  }

  it should "forward messages from the Client to the LobbyManager(server)" in {
    val serverProxy = spawn(actorServerProxy.proxyInLobby())
    serverProxy ! Create
    lobbyManagerProbe expectMessage CreateRoom(serverProxy.ref)

    serverProxy ! Join(roomId)
    lobbyManagerProbe expectMessage JoinRoom(roomId, serverProxy.ref)
  }

  "ServerProxy in game" should "forward messages from the GameManager(server) to the Client" in {
    val serverProxy = spawn(actorServerProxy.proxyInGame(gameManagerProbe.ref))
    serverProxy ! GameUpdate(color, game, request)
    clientProbe.expectMessageType[GameUpdate]

    serverProxy ! GameEnd(Win(color), game)
    clientProbe.expectMessageType[GameEnd]
  }

  it should "forward errors from the LobbyManager(server) to the Client" in {
    val serverProxy = spawn(actorServerProxy.proxyInGame(gameManagerProbe.ref))
    serverProxy ! FailedMove("err", move)
    clientProbe.expectMessageType[FailedMove]
  }

  it should "forward messages from the Client to the GameManager(server)" in {
    val serverProxy = spawn(actorServerProxy.proxyInGame(gameManagerProbe.ref))
    serverProxy ! InputMove(move)
    gameManagerProbe expectMessage DoMove(move, serverProxy.ref)

    serverProxy ! Forfeit
    gameManagerProbe expectMessage ForfeitGame(serverProxy.ref)
  }

}
