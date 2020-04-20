package it.scalachess.ai.level

import it.scalachess.ai.movesearch.MinimaxNode
import it.scalachess.core.{Black, Color, White}
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.pieces.{Bishop, King, Knight, Pawn, Piece, Queen, Rook}

/**
 * The level four A.I.'s uses a minimax depth = 2, moreover this A.I. has a pieces' position evaluation.
 */
class LevelFour() extends LevelThree {

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(MinimaxNode(board, history), aiPlayer, evaluatePiecesAndTheirPosInBoard))
  }

  /**
   * Evaluates board relying on pieces' type importance, their position, and the player's color,
   * @param board the board to evaluate
   * @param aiPlayer the color of the AI player
   * @return the evaluation of the board
   */
  final protected def evaluatePiecesAndTheirPosInBoard(board: Board, aiPlayer: Color): Double =
    board.pieces
      .map(piece =>
        piece._2.color match {
          case aiPlayer.other => - evaluatePiece(piece._2.pieceType) - evaluatePiecePos(piece._1, piece._2)
          case _            => evaluatePiece(piece._2.pieceType) + evaluatePiecePos(piece._1, piece._2)
        })
      .toList
      .sum

  /**
   * Evaluates a piece position relying on the piece type and color.
   * @param pos the position to evaluate
   * @param piece the piece to evaluate
   * @return the evaluation of the position
   */
  final protected def evaluatePiecePos(pos: Position, piece: Piece): Double = {
    def posValueByColor(color: Color, whiteValue: Double, blackValue: Double): Double = {
      color match {
        case White => whiteValue
        case Black => blackValue
      }
    }
    piece.pieceType match {
      case Pawn   => posValueByColor(piece.color, whitePawnPosValue(pos.row-1)(pos.col-1), blackPawnPositionsValue(pos.row-1)(pos.col-1))
      case Knight => knightPosValue(pos.row-1)(pos.col-1)
      case Bishop => posValueByColor(piece.color, whiteBishopPosValue(pos.row-1)(pos.col-1), blackBishopPosValue(pos.row-1)(pos.col-1))
      case Rook   => posValueByColor(piece.color, whiteRookPosValue(pos.row-1)(pos.col-1), blackRookPosValue(pos.row-1)(pos.col-1))
      case Queen  => queenPosValue(pos.row-1)(pos.col-1)
      case King   => posValueByColor(piece.color, whiteKingPosValue(pos.row-1)(pos.col-1), blackKingPosValue(pos.row-1)(pos.col-1))
      case _ =>
        assert(assertion = false, s"The AI doesn't know the value of this piece: ${piece.pieceType}")
        0
    }
  }

  final protected val whitePawnPosValue: Array[Array[Double]] = Array(
    Array(0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0),
    Array(5.0,  5.0,  5.0,  5.0,  5.0,  5.0,  5.0,  5.0),
    Array(1.0,  1.0,  2.0,  3.0,  3.0,  2.0,  1.0,  1.0),
    Array(0.5,  0.5,  1.0,  2.5,  2.5,  1.0,  0.5,  0.5),
    Array(0.0,  0.0,  0.0,  2.0,  2.0,  0.0,  0.0,  0.0),
    Array(0.5, -0.5, -1.0,  0.0,  0.0, -1.0, -0.5,  0.5),
    Array(0.5,  1.0,  1.0, -2.0, -2.0,  1.0,  1.0,  0.5),
    Array(0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0))
  final protected val blackPawnPositionsValue: Array[Array[Double]] = whitePawnPosValue.reverse

  final protected val knightPosValue: Array[Array[Double]] = Array(
    Array(-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0),
    Array(-4.0, -2.0,  0.0,  0.5,  0.5,  0.0, -2.0, -4.0),
    Array(-3.0,  0.0,  1.0,  1.5,  1.5,  1.0,  0.0, -3.0),
    Array(-3.0,  0.5,  1.5,  2.0,  2.0,  1.5,  0.5, -3.0),
    Array(-3.0,  0.5,  1.5,  2.0,  2.0,  1.5,  0.5, -3.0),
    Array(-3.0,  0.0,  1.0,  1.5,  1.5,  1.0,  0.0, -3.0),
    Array(-4.0, -2.0,  0.0,  0.5,  0.5,  0.0, -2.0, -4.0),
    Array(-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0))

  final protected val whiteBishopPosValue: Array[Array[Double]] = Array(
    Array(-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0),
    Array(-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-1.0,  0.0,  0.5,  1.0,  1.0,  0.5,  0.0, -1.0),
    Array(-1.0,  0.5,  0.5,  1.0,  1.0,  0.5,  0.5, -1.0),
    Array(-1.0,  0.0,  1.0,  1.0,  1.0,  1.0,  0.0, -1.0),
    Array(-1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0, -1.0),
    Array(-1.0,  0.5,  0.0,  0.0,  0.0,  0.0,  0.5, -1.0),
    Array(-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0))
  final protected val blackBishopPosValue: Array[Array[Double]] = whiteBishopPosValue.reverse

  final protected val whiteRookPosValue: Array[Array[Double]] = Array(
    Array( 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0),
    Array( 0.5,  1.0,  1.0,  1.0,  1.0,  1.0,  1.0,  0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array(-0.5,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -0.5),
    Array( 0.0,  0.0,  0.0,  0.5,  0.5,  0.0,  0.0,  0.0))
  final protected val blackRookPosValue: Array[Array[Double]] = whiteRookPosValue.reverse

  final protected val queenPosValue: Array[Array[Double]] = Array(
    Array(-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0),
    Array(-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-1.0,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -1.0),
    Array(-0.5,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -0.5),
    Array(-0.5,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -0.5),
    Array(-1.0,  0.0,  0.5,  0.5,  0.5,  0.5,  0.0, -1.0),
    Array(-1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, -1.0),
    Array(-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0))

  final protected val whiteKingPosValue: Array[Array[Double]] = Array(
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0),
    Array(-2.0, -3.0, -3.0, -4.0, -4.0, -3.0, -3.0, -2.0),
    Array(-1.0, -2.0, -2.0, -2.0, -2.0, -2.0, -2.0, -1.0),
    Array( 2.0,  2.0,  0.0,  0.0,  0.0,  0.0,  2.0,  2.0),
    Array( 2.0,  3.0,  1.0,  0.0,  0.0,  1.0,  3.0,  2.0))
  final protected val blackKingPosValue: Array[Array[Double]] = whiteKingPosValue.reverse

}