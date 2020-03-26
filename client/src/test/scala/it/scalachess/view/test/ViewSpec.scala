package it.scalachess.view.test

import it.scalachess.client.view.{ CLI, CliView, ViewFactory }
import it.scalachess.core.board.Board
import org.scalatest.{ FlatSpec, Inspectors, Matchers }

class ViewSpec extends FlatSpec with Matchers with Inspectors {

  val basicView    = ViewFactory(CLI)
  val board: Board = Board.defaultBoard()
  val outputView: String = s"8 [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]\n" +
  s"7 [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]\n" +
  s"6 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"5 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"4 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"3 [   ][   ][   ][   ][   ][   ][   ][   ]\n" +
  s"2 [ P ][ P ][ P ][ P ][ P ][ P ][ P ][ P ]\n" +
  s"1 [ R ][ N ][ B ][ Q ][ K ][ B ][ N ][ R ]\n" +
  s"    A    B    C    D    E    F    G    H  "

  "A basic view" should "be an instance of BasicView" in {
    basicView equals CliView shouldEqual true
  }

  it should "be visualized as" in {
    basicView.getStringFromBoard(board) shouldEqual outputView
  }
}
