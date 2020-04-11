package it.scalachess.ai.level
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }

case class LevelOne() extends Level {

  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove =
    new MoveGenerator(board: Board, player: Color, history: Seq[FullMove])
      .allMoves()
      .map(move => evaluateBoardByPieceValue(board(move.validMove.boardChanges), player) -> move)
      .toMap
      .max
      ._2

  def evaluateBoardByPieceValue(board: Board, player: Color): Double =
    board.pieces
      .filter(_._2.color == player)
      .map(piece => pieceValue(piece._2.pieceType))
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

  // pieces' value estimated by Alan Turing
  private val pawnValue   = 1
  private val knightValue = 3
  private val bishopValue = 3.5
  private val rookValue   = 5
  private val queenValue  = 10
  private val kingValue   = 1000

}
