package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import scala.util.Random

case class LevelZero() extends Level {

  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove = {
    val moves = new MoveGenerator(board: Board, player: Color, history: Seq[FullMove]).allMoves()
    moves(Random.nextInt(moves.size))
  }

}
