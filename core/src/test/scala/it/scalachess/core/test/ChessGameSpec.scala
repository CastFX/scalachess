package it.scalachess.core.test

import it.scalachess.core.ChessGame
import it.scalachess.core.colors.White
import org.scalatest.FlatSpec

class ChessGameSpec extends FlatSpec {
  val game = ChessGame.standard()

  "A standard ChessGame" should "have a non-empty board" in {
    assert(game.board.pieces.nonEmpty)
  }

  it should "have a starting white player at turn 0" in {
    assert(game.player === White)
    assert(game.turn == 0)
  }
}
