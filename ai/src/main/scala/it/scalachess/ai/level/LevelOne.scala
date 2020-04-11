package it.scalachess.ai.level
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove

case class LevelOne() extends Level {
  override def generateSmartMove(board: Board, player: Color, history: Seq[FullMove]): FullMove = ???
}
