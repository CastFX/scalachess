package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

case class LevelTwo() extends Level {

  private val levelOne = LevelOne()

  override def apply(board: Board, color: Color, history: Seq[FullMove]): FullMove = ???

  def minimaxAlgorithm() = ???

}
