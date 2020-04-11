package it.scalachess.view.test

import it.scalachess.client.view.{ CLI, CliView, ViewFactory }
import it.scalachess.core.board.Board
import org.scalatest.{ FlatSpec, Inspectors, Matchers }
import akka.actor.testkit.typed.scaladsl.{ ActorTestKit, LoggingTestKit }
import it.scalachess.core.{ Draw, Result, White, Win, WinByForfeit }
import org.slf4j.event.Level
class ViewSpec extends FlatSpec with Matchers with Inspectors {

  val testKit: ActorTestKit = ActorTestKit()
  import testKit._
  var loggingTest: LoggingTestKit = LoggingTestKit.empty.withLogLevel(Level.INFO)
  val cliView                     = ViewFactory(CLI)
  val board: Board                = Board.defaultBoard()
  val outputView: String = s"8 [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]\n" +
  s"7 [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]\n" +
  s"6 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"5 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"4 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"3 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"2 [ P ][ P ][ P ][ P ][ P ][ P ][ P ][ P ]\n" +
  s"1 [ R ][ N ][ B ][ Q ][ K ][ B ][ N ][ R ]\n" +
  s"    A    B    C    D    E    F    G    H  "

  "A cli view" should "be an instance of CliView" in {
    cliView equals CliView shouldEqual true
  }

  it should "be visualized as" in {
    loggingTest = loggingTest.withMessageContains(outputView)
    loggingTest.expect(cliView.showBoard(board))
  }

  it should "be able to show a message when requested" in {
    loggingTest = loggingTest.withMessageContains("The winner is: Me.")
    loggingTest.expect(cliView.showMessage("The winner is: Me."))
  }

  it should "be able to show a result when request" in {
    val win: Result        = Win(White)
    val winResultMessage   = "White wins"
    val winForfeit: Result = WinByForfeit(White)
    val winForfeitMessage  = "White wins by forfeit"
    val draw: Result       = Draw
    val drawMessage        = "Draw"
    loggingTest = loggingTest.withMessageContains(winResultMessage)
    loggingTest.expect(cliView.showResult(win))
    loggingTest = loggingTest.withMessageContains(winForfeitMessage)
    loggingTest.expect(cliView.showResult(winForfeit))
    loggingTest = loggingTest.withMessageContains(drawMessage)
    loggingTest.expect(cliView.showResult(draw))
  }
}
