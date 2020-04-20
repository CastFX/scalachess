package it.scalachess.server.test

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import it.scalachess.core._
import it.scalachess.core.logic.{ Result, Win, WinByForfeit }
import it.scalachess.server.LobbyManager.TerminateGame
import it.scalachess.server.{ GameManager, Room }
import it.scalachess.util.NetworkErrors.FailedMove
import it.scalachess.util.NetworkMessages._
import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers, OptionValues }

class GameManagerSpec extends FlatSpec with BeforeAndAfterAll with Matchers with OptionValues {
  private val testKit      = ActorTestKit()
  private val lobbyManager = testKit.createTestProbe[LobbyMessage]
  private val client1      = testKit.createTestProbe[ClientMessage]
  private val client2      = testKit.createTestProbe[ClientMessage]
  private val roomId       = "123"
  private val room         = Room(roomId, (client1.ref, client2.ref))

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "A GameManager" should "send a GameStart to the connected Clients" in {
    testKit.spawn(GameManager(room, lobbyManager.ref))
    val gs1 = client1.expectMessageType[GameStart]
    val gs2 = client2.expectMessageType[GameStart]
    gs1.game should equal(gs2.game)
    gs1.color should equal(gs2.color.other)
  }

  it should "send GameUpdate to both clients after one of them moves" in {
    val gameManager = testKit.spawn(GameManager(room, lobbyManager.ref))
    val players = Map(
      client1.expectMessageType[GameStart].color -> client1.ref,
      client2.expectMessageType[GameStart].color -> client2.ref
    )

    val initialMove = "f3"
    gameManager ! DoMove(initialMove, players(White))
    val expectedGame: ChessGame = ChessGame.standard().apply(initialMove).toOption.value
    assertClientsReceivesCorrectGame(expectedGame)
  }

  it should "terminate after checkmates" in {
    assertGameFlow(Seq("f3", "e5", "g4", "Qh4#"), Win(Black))
    assertGameFlow(Seq("e4", "f6", "d3", "g5", "Qh5#"), Win(White))
  }

  it should "terminate after a forfeit by either player" in {
    def assertForfeit(color: Color): Unit = {
      val gameManager = testKit.spawn(GameManager(room, lobbyManager.ref))
      val players = Map(
        client1.expectMessageType[GameStart].color -> client1.ref,
        client2.expectMessageType[GameStart].color -> client2.ref
      )
      gameManager ! ForfeitGame(players(color).ref)
      val chessGame = ChessGame.standard().end(WinByForfeit(color.other))
      assertGameEnd(WinByForfeit(color.other), chessGame)
    }
    assertForfeit(White)
    assertForfeit(Black)
  }

  //Todo result = draw

  "GameManager" should "send FailedMove if the client tries to make an illegal move" in {
    val gameManager = testKit.spawn(GameManager(room, lobbyManager.ref))
    val players = Map(
      client1.expectMessageType[GameStart].color -> client1,
      client2.expectMessageType[GameStart].color -> client2
    )
    val wrongMove = "a2"
    gameManager ! DoMove("a2", players(White).ref)

    val err = players(White).expectMessageType[FailedMove]
    err.move should equal(wrongMove)
  }

  private def assertClientsReceivesCorrectGame(expected: ChessGame): Unit = {
    val update1 = client1.expectMessageType[GameUpdate]
    val update2 = client2.expectMessageType[GameUpdate]
    update1.game should equal(expected)
    update1.game should equal(update2.game)
    update1.color should equal(update2.color.other)
  }

  private def assertGameEnd(expectedResult: Result, chessGame: ChessGame): Unit = {
    client1.expectMessage(GameEnd(expectedResult, chessGame))
    client2.expectMessage(GameEnd(expectedResult, chessGame))

    val terminateGame = lobbyManager.expectMessageType[TerminateGame]
    terminateGame.game.gameStatus should equal(expectedResult)
    terminateGame.roomId should equal(roomId)
  }

  private def assertGameFlow(moves: Seq[String], expectedResult: Result): Unit = {
    val gameManager = testKit.spawn(GameManager(room, lobbyManager.ref))
    val players = Map(
      client1.expectMessageType[GameStart].color -> client1.ref,
      client2.expectMessageType[GameStart].color -> client2.ref
    )
    var game = ChessGame.standard()
    moves.foreach { move =>
      gameManager ! DoMove(move, players(game.player))
      game = game(move).toOption.value
      assertClientsReceivesCorrectGame(game)
    }
    assertGameEnd(expectedResult, game)
  }
}
