package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.annotation.tailrec

/**
 * The level two AI plays the best move following two principles:
 * [1] it cares about value based on piece importance (it uses the same evaluation of level one);
 * [2] it analyse all the possible next moves, relying on the minimax algorithm.
 */
class LevelTwo() extends LevelOne {

  protected val minimaxDepth   = 3
  protected val checkmateValue = 9999.0

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    opponentNotInCheckmate(board, aiPlayer, history)
    randomMove(movesWithMaxEvaluation(generateMovesWithMinimaxEval(board, aiPlayer, history, minimaxDepth)))
  }

  /**
   * Generates the moves and their evaluation using the minimax algorithm.
   * @param board the board on which computes the move generation and evaluation
   * @param aiPlayer the color of the AI player
   * @param history the moves history played on the board
   * @param depth the depth of the minimax algorithm
   * @return the map containing the moves and the relative evaluations
   */
  protected def generateMovesWithMinimaxEval(board: Board,
                                                  aiPlayer: Color,
                                                  history: Seq[FullMove],
                                                  depth: Int): Map[FullMove, Double] = {
    require(depth > 0, "The minimax algorithm should computes at least the consequences of his first future move")
    new MoveGenerator(board, aiPlayer, history)
      .allMoves()
      .map(fullMove => {
        fullMove -> minimax(board(fullMove.validMove.boardChanges),
          history :+ fullMove,
          depth - 1,
          aiPlayer.other,
          aiPlayer,
          -checkmateValue,
          checkmateValue)
      })
      .toMap
  }
  /**
   * Evaluates a board relying on the minimax algorithm with alpha-beta pruning.
   *
   * @param board the board to evaluate
   * @param history the moves history played on the board
   * @param depth the depth of the minimax algorithm
   * @param currentPlayer the active player color during the current minimax turn simulation
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @return the evaluation of the board
   */
  protected def minimax(board: Board,
              history: Seq[FullMove],
              depth: Int,
              currentPlayer: Color,
              maximizingPlayer: Color,
              alpha: Double,
              beta: Double): Double =
    depth match {
      case 0 => evaluatePiecesInBoard(board, maximizingPlayer)
      case _ =>
        val allPossibleMoves = new MoveGenerator(board, currentPlayer, history).allMoves()
        if (allPossibleMoves.isEmpty) // this move leads one of the 2 player in checkmate
          decideCurrentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue)
        else {
          alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer,
            alpha, beta, allPossibleMoves,
            decideCurrentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue))
        }
    }
  protected def decideCurrentPlayerValue(currentPlayer: Color,
                                         maximizingPlayer: Color,
                                         minimizingPlayerValue: Double,
                                         maximizingPlayerValue: Double): Double =
    currentPlayer match {
      case `maximizingPlayer` => maximizingPlayerValue
      case _                  => minimizingPlayerValue
    }
  @tailrec
  private def alphaBetaPruning(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                               alpha: Double, beta: Double, allPossibleMoves: List[FullMove], bestMoveEval: Double): Double = {
    def recallMinimax(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                      alpha: Double, beta: Double, move: FullMove): Double = {
      minimax(board(move.validMove.boardChanges),
        history :+ move,
        depth - 1,
        currentPlayer.other,
        maximizingPlayer,
        alpha,
        beta)
    }
    if(allPossibleMoves.isEmpty) bestMoveEval
    else {
      var bestMoveEvalUpdated = bestMoveEval
      val move = allPossibleMoves.head
      currentPlayer match {
        case `maximizingPlayer` =>
          bestMoveEvalUpdated = math.max(bestMoveEvalUpdated,
            recallMinimax(board, history, depth, currentPlayer, maximizingPlayer, alpha, beta, move))
        case _ =>
          bestMoveEvalUpdated = math.min(bestMoveEvalUpdated,
            recallMinimax(board, history, depth, currentPlayer, maximizingPlayer, alpha, beta, move))
      }
      val alphaOrBeta = decideCurrentPlayerValue(currentPlayer, maximizingPlayer, math.min(bestMoveEvalUpdated, beta), math.max(bestMoveEvalUpdated, alpha))
      currentPlayer match {
        case `maximizingPlayer` =>
          if (alphaOrBeta >= beta) bestMoveEvalUpdated
          else alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, alphaOrBeta, beta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
        case _                  =>
          if (alpha >= alphaOrBeta) bestMoveEvalUpdated
          else alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, alpha, alphaOrBeta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
      }
    }
  }




}
