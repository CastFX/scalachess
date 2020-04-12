package it.scalachess.ai.level
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }

case class LevelOne() extends Level {

  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove = {
    val movesEvaluated = new MoveGenerator(board: Board, player: Color, history: Seq[FullMove])
      .allMoves()
      .map(move => (move, evaluateBoardByPieceValue(board(move.validMove.boardChanges), player)))
    movesEvaluated.find(_._2 == (movesEvaluated.map(_._2).max)) match {
      case Some(moveEvaluated) => moveEvaluated._1
      case _                   => movesEvaluated.head._1
    }
  }

  def evaluateBoardByPieceValue(board: Board, player: Color): Double =
    board.pieces
      .map {
        case piece @ player.other => -pieceValue(piece._2.pieceType)
        case piece @ _            => pieceValue(piece._2.pieceType)
      }
      .fold(0.0)(_ + _)

  def pieceValue(pieceType: PieceType): Double =
    pieceType match {
      case Pawn   => pawnValue
      case Knight => knightValue
      case Bishop => bishopValue
      case Rook   => rookValue
      case Queen  => queenValue
      case King   => kingValue
      case _      => 0
    }

  private val pawnValue   = 10
  private val knightValue = 30
  private val bishopValue = 35
  private val rookValue   = 50
  private val queenValue  = 100
  private val kingValue   = 1000

}
