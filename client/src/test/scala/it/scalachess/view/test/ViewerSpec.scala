package it.scalachess.view.test

import akka.actor.testkit.typed.scaladsl.{ ActorTestKit, LoggingTestKit }
import akka.actor.typed.ActorRef
import it.scalachess.client.remote_client.{ Client, ClientCommands }
import it.scalachess.client.view.ViewCommands.{ ShowBoard, ShowMessage, ShowResult }
import it.scalachess.client.view.{ CLI, ViewCommands, Viewer }
import it.scalachess.core.{ Draw, White, Win, WinByForfeit }
import it.scalachess.core.board.Board
import it.scalachess.core.logic.{ Draw, Result, Win, WinByForfeit }
import it.scalachess.util.NetworkMessages
import org.scalatest._
import org.slf4j.event.Level

class ViewerSpec extends FlatSpec with BeforeAndAfterAll with Matchers with OptionValues with Inspectors {
  val testKit: ActorTestKit = ActorTestKit()
  import testKit._
  override def afterAll(): Unit = shutdownTestKit()

  val address: String = "127.0.0.1:25555"
  val board: Board    = Board.defaultBoard()
  val outputViewBoard: String = s"8 [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]\n" +
  s"7 [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]\n" +
  s"6 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"5 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"4 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"3 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"2 [ P ][ P ][ P ][ P ][ P ][ P ][ P ][ P ]\n" +
  s"1 [ R ][ N ][ B ][ Q ][ K ][ B ][ N ][ R ]\n" +
  s"    A    B    C    D    E    F    G    H  "

  val message = "The winner is: Me."

  val win: Result        = Win(White)
  val winResultMessage   = "White wins"
  val winForfeit: Result = WinByForfeit(White)
  val winForfeitMessage  = "White wins by forfeit"
  val draw: Result       = Draw
  val drawMessage        = "Draw"

  var loggingTest: LoggingTestKit                     = LoggingTestKit.empty.withLogLevel(Level.INFO).withOccurrences(Int.MaxValue)
  val client: ActorRef[NetworkMessages.ClientMessage] = spawn(Client(address))
  val viewer: ActorRef[ViewCommands.ViewMessage]      = spawn(Viewer(client, CLI))

  "A Viewer" should "be able to show what is requested" in {

    loggingTest = loggingTest.withMessageContains(outputViewBoard)
    loggingTest.expect(viewer ! ShowBoard(board))
    loggingTest = loggingTest.withMessageContains(message)
    loggingTest.expect(viewer ! ShowMessage(message))
    loggingTest = loggingTest.withMessageContains(winResultMessage)
    loggingTest.expect(viewer ! ShowResult(win))
    loggingTest = loggingTest.withMessageContains(winForfeitMessage)
    loggingTest.expect(viewer ! ShowResult(winForfeit))
    loggingTest = loggingTest.withMessageContains(drawMessage)
    loggingTest.expect(viewer ! ShowResult(draw))
  }
}
