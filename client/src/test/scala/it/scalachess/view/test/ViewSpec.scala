package it.scalachess.view.test

import it.scalachess.client.view.{ CLI, CliView, ViewFactory }
import it.scalachess.core.board.Board
import org.scalatest.{ FlatSpec, Inspectors, Matchers }

class ViewSpec extends FlatSpec with Matchers with Inspectors {

  val basicView    = ViewFactory(CLI)
  val board: Board = Board.defaultBoard()
  val whitePawn    = '♙'
  val blackPawn    = '♟'
  val whiteRook    = '♖'
  val blackRook    = '♜'
  val whiteKnight  = '♘'
  val blackKnight  = '♞'
  val whiteBishop  = '♗'
  val blackBishop  = '♝'
  val whiteKing    = '♔'
  val blackKing    = '♚'
  val whiteQueen   = '♕'
  val blackQueen   = '♛'

  val piecesPerType: Map[Char, Int] = Map(
    whitePawn   -> 8,
    blackPawn   -> 8,
    whiteRook   -> 2,
    blackRook   -> 2,
    whiteKnight -> 2,
    blackKnight -> 2,
    whiteBishop -> 2,
    blackBishop -> 2,
    whiteKing   -> 1,
    blackKing   -> 1,
    whiteQueen  -> 1,
    blackQueen  -> 1
  )

  "A basic view" should "be an instance of BasicView" in {
    basicView.isInstanceOf[CliView] shouldEqual true
  }

  it should
  s"have ${piecesPerType(whitePawn)} white pawns, " +
  s"${piecesPerType(blackPawn)} black pawns, " +
  s"${piecesPerType(whiteRook)} white rooks, " +
  s"${piecesPerType(blackRook)} black rooks, " +
  s"${piecesPerType(whiteKnight)} white knights, " +
  s"${piecesPerType(blackKnight)} black knights, " +
  s"${piecesPerType(whiteBishop)} white bishops, " +
  s"${piecesPerType(blackBishop)} black bishops, " +
  s"${piecesPerType(whiteKing)} white kings, " +
  s"${piecesPerType(blackKing)} black kings, " +
  s"${piecesPerType(whiteQueen)} white queens" +
  s"${piecesPerType(blackQueen)} black queens" in {

    val piecesCounts = basicView
      .getStringFromBoard(board)
      .groupBy(identity)
      .mapValues(_.length)
      .toSet
      .filter(x => piecesPerType.contains(x._1))

    piecesCounts shouldEqual piecesPerType.toSet
  }

}
