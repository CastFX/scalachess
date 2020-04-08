import akka.actor.testkit.typed.scaladsl.ActorTestKit
import it.scalachess.server.LobbyManager
import it.scalachess.util.NetworkErrors.{ RoomFull, RoomNotFound }
import it.scalachess.util.NetworkMessages._
import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers }

class LobbyManagerSpec extends FlatSpec with BeforeAndAfterAll with Matchers {
  val testKit = ActorTestKit()
  import testKit._
  override def afterAll(): Unit = testKit.shutdownTestKit()

  "Lobby Manager" should "should create a Room after receiving a Create + Join messages and reply with a GameStart to both clients" in {
    val lobbyManager = spawn(LobbyManager())
    val client1      = createTestProbe[ClientMessage]()
    val client2      = createTestProbe[ClientMessage]()

    lobbyManager ! CreateRoom(client1.ref)
    val roomId: String = client1.expectMessageType[RoomId].id
    lobbyManager ! JoinRoom(roomId, client2.ref)
    val (gs1, gs2) = (client1.expectMessageType[GameStart], client2.expectMessageType[GameStart])
    gameStartsShouldBeEqual(gs1, gs2)
  }

  private def gameStartsShouldBeEqual(gs1: GameStart, gs2: GameStart): Unit = {
    gs1.game should equal(gs2.game)
    gs1.color should equal(gs2.color.other)
  }

  it should "reply with RoomNotFound if joining with an invalid id" in {
    val lobbyManager = spawn(LobbyManager())
    val client1      = createTestProbe[ClientMessage]()

    val fakeRoomId = "abc"
    lobbyManager ! JoinRoom(fakeRoomId, client1.ref)
    client1.expectMessage(RoomNotFound(fakeRoomId))
  }

  it should "should be able to host multiple rooms" in {
    val lobbyManager = spawn(LobbyManager())
    val rooms        = 10
    val clients = (1 to rooms).map { _ =>
      (createTestProbe[ClientMessage](), createTestProbe[ClientMessage]())
    }.toMap

    clients.foreach {
      case (creating, joining) =>
        lobbyManager ! CreateRoom(creating.ref)
        val id = creating.expectMessageType[RoomId].id
        lobbyManager ! JoinRoom(id, joining.ref)

        val (gs1, gs2) = (creating.expectMessageType[GameStart], joining.expectMessageType[GameStart])
        gameStartsShouldBeEqual(gs1, gs2)
    }
  }

  "A client" should "not be able to join a Full Room" in {
    val lobbyManager = spawn(LobbyManager())
    val client1      = createTestProbe[ClientMessage]()
    val client2      = createTestProbe[ClientMessage]()
    val client3      = createTestProbe[ClientMessage]()

    lobbyManager ! CreateRoom(client1.ref)
    val roomId: String = client1.expectMessageType[RoomId].id
    lobbyManager ! JoinRoom(roomId, client2.ref)
    lobbyManager ! JoinRoom(roomId, client3.ref)
    client3.expectMessage(RoomFull(roomId))
  }
}
