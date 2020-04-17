package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }

/**
 * The level one AI plays the move which capture the more important piece
 * (if two or more moves capture that piece, the move is choosen randomly).
 */
class LevelOne() extends LevelZero {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    opponentNotInCheckmate(board, aiPlayer, history)
    randomMove(movesWithMaxEvaluation(generateMovesWithEvaluation(board, aiPlayer, history)))
  }

  /**
   * Returns the moves which have the highest evaluation
   * @param movesEvaluated the map containing all the possibles moves and the relative evaluations
   * @return the moves list having the highest evaluation
   */
  protected def movesWithMaxEvaluation(movesEvaluated: Map[FullMove, Double]): List[FullMove] = {
    movesEvaluated.filter(_._2 == movesEvaluated.values.max).keys.toList
  }

  /**
   * Generates the moves and their evaluation.
   * @param board the board on which computes the move generation and evaluation
   * @param aiPlayer the color of the AI player
   * @param history the moves history played on the board
   * @return the map containing the moves and the relative evaluations
   */
  protected def generateMovesWithEvaluation(board: Board,
                                                 aiPlayer: Color,
                                                 history: Seq[FullMove]): Map[FullMove, Double] =
    new MoveGenerator(board: Board, aiPlayer: Color, history: Seq[FullMove])
      .allMoves()
      .map(move => move -> evaluatePiecesInBoard(board(move.validMove.boardChanges), aiPlayer))
      .toMap

  /**
   * Evaluates pieces in a board relying on a player's color
   * @param board the board on which computes the evaluation
   * @param player the color of the AI player
   * @return the evaluation of the board
   */
  protected def evaluatePiecesInBoard(board: Board, player: Color): Double =
    board.pieces
      .map(piece =>
        piece._2.color match {
          case player.other => -evaluatePiece(piece._2.pieceType)
          case _            => evaluatePiece(piece._2.pieceType)
      })
      .toList
      .sum

  /**
   * Evaluates a piece relying on his type
   * @param pieceType the type of the piece to evaluate
   * @return the evalutation of that piece type
   */
  protected def evaluatePiece(pieceType: PieceType): Double =
    pieceType match {
      case Pawn   => pawnValue
      case Knight => knightValue
      case Bishop => bishopValue
      case Rook   => rookValue
      case Queen  => queenValue
      case King   => kingValue
      case _ =>
        assert(assertion = false, s"The AI doesn't know the value of this piece: $pieceType")
        0
    }
  protected val pawnValue   = 10
  protected val knightValue = 30
  protected val bishopValue = 35
  protected val rookValue   = 50
  protected val queenValue  = 100
  protected val kingValue   = 1000

}
