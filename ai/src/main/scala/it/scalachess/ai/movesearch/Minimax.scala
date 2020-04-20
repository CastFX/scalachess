package it.scalachess.ai.movesearch

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.annotation.tailrec

/**
 * Implements the minimax algorithm with alpha-beta pruning.
 */
trait Minimax {

  protected val minimaxDepth = 1
  final protected val checkmateValue = 9999.0

  /**
   * Generates the moves with the relative evaluation, calculated using the minimax algorithm.
   * @param rootNode is the root node where the minimax search starts
   * @param aiPlayer the color of the AI player
   * @param evaluationFunc the minimax will use this function to evaluate a node located on the minimax horizon
   * @return the map containing the next possible moves and their relative valuation
   */
  final protected def minimax(rootNode: MinimaxNode, aiPlayer: Color, evaluationFunc: (Board, Color) => Double): Map[FullMove, Double] = {
    /**
     * The first minimax call on the root node.
     * @param depth the depth used by the minimax algorithm
     */
    def minimaxRoot(rootNode: MinimaxNode, aiPlayer: Color, evaluationFunc: (Board, Color) => Double, depth: Int): Map[FullMove, Double] = {
      require(depth > 0, "The minimax algorithm should computes at least the consequences of his first future move")
      new MoveGenerator(rootNode.board, aiPlayer, rootNode.history)
        .allMoves()
        .map(fullMove => {
          fullMove ->
            minimaxGoesDeep(rootNode, fullMove, depth, aiPlayer, aiPlayer, evaluationFunc, AlphaBeta(-checkmateValue, checkmateValue))
        })
        .toMap
    }
    minimaxRoot(rootNode, aiPlayer, evaluationFunc, minimaxDepth)
  }

  /**
   * Calls the minimax algorithm with alpha-beta pruning, decreasing the depth.
   * @param fatherNode the node father of the next minimax call
   * @param fullMove the move that will generate the son node
   * @return the result of the evaluation
   */
  protected def minimaxGoesDeep(fatherNode: MinimaxNode, fullMove: FullMove, depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                evaluationFunc: (Board, Color) => Double, alphaBeta: AlphaBeta): Double = {
    require(depth > 0, "Error: the minimax can't be called with a depth <= 0!")
    minimaxEval(MinimaxNode(fatherNode.board(fullMove.validMove.boardChanges), fatherNode.history :+ fullMove),
      depth - 1,
      currentPlayer.other,
      maximizingPlayer,
      evaluationFunc,
      alphaBeta)
  }

  /**
   * Evaluates a board relying on the minimax algorithm with alpha-beta pruning.
   * @param node the node to evaluate
   * @param depth the current depth of the minimax algorithm
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param evaluationFunc this function will be used to evaluate a node located on the minimax horizon
   * @param alphaBeta alpha-beta pruning values
   * @return the evaluation of the node
   */
  private def minimaxEval(node: MinimaxNode, depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                          evaluationFunc: (Board, Color) => Double, alphaBeta: AlphaBeta): Double = {
    depth match {
      case 0 =>
        // an horizon node of the minimax search
        evaluationFunc(node.board, maximizingPlayer)
      case _ =>
        val allPossibleMoves = new MoveGenerator(node.board, currentPlayer, node.history).allMoves()
        if (allPossibleMoves.isEmpty) {
          // a terminal node of minimax search: one of the two player is in checkmate
          currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue)
        } else {
          // an intermediate node of the minimax search
          alphaBetaPruningSearch(node, depth, currentPlayer, maximizingPlayer, evaluationFunc, allPossibleMoves,
            alphaBeta, currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue))
        }
    }
  }

  /**
   * Decides which value use during a minimax iteration.
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param minimizingPlayerValue the value corresponding to the minimizing player
   * @param maximizingPlayerValue the value corresponding to the maximizing player
   * @return the value corresponding to the current player
   */
  final protected def currentPlayerValue(currentPlayer: Color, maximizingPlayer: Color,
                                         minimizingPlayerValue: Double, maximizingPlayerValue: Double): Double =
    currentPlayer match {
      case `maximizingPlayer` => maximizingPlayerValue
      case _                  => minimizingPlayerValue
    }

  /**
   * A unique procedure used to perform alpha-beta pruning, both for the minimizing player and for the maximizing player.
   * @param fatherNode the father node on which the alpha-beta pruning search is working
   * @param depth the current depth of minimax algorithm
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param allPossibleMoves all playable moves that needs to be considered during this alpha-beta pruning iteration. Each one will lead to a new node
   * @param alphaBeta the alpha-beta values used by the pruning search
   * @param bestMoveEval the current best move evaluation found so far by the alpha-beta pruning search
   * @return the evaluation of the father node taken as input
   */
  @tailrec
  final protected def alphaBetaPruningSearch(fatherNode: MinimaxNode, depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                             evaluationFunc: (Board, Color) => Double, allPossibleMoves: Seq[FullMove],
                                             alphaBeta: AlphaBeta, bestMoveEval: Double): Double = {
    if(allPossibleMoves.isEmpty) {
      // all nodes have been explored
      bestMoveEval
    } else {
      var bestMoveEvalUpdated = bestMoveEval
      val move = allPossibleMoves.head
      // 1° alpha-beta pruning step: updates the best value found so far
      currentPlayer match {
        case `maximizingPlayer` =>
          bestMoveEvalUpdated = math.max(bestMoveEvalUpdated,
            minimaxGoesDeep(fatherNode, move, depth, currentPlayer, maximizingPlayer, evaluationFunc, alphaBeta))
        case _ =>
          bestMoveEvalUpdated = math.min(bestMoveEvalUpdated,
            minimaxGoesDeep(fatherNode, move, depth, currentPlayer, maximizingPlayer, evaluationFunc, alphaBeta))
      }
      // 2° alpha-beta pruning step: updates alpha or beta valueS
      val alphaOrBeta = currentPlayerValue(currentPlayer, maximizingPlayer,
        math.min(bestMoveEvalUpdated, alphaBeta.beta), math.max(bestMoveEvalUpdated, alphaBeta.alpha))
      // 3° alpha-beta pruning phase: prune or continue the evaluation
      currentPlayer match {
        case `maximizingPlayer` =>
          if (alphaOrBeta >= alphaBeta.beta) bestMoveEvalUpdated
          else alphaBetaPruningSearch(fatherNode, depth, currentPlayer, maximizingPlayer, evaluationFunc,
            allPossibleMoves.drop(1), AlphaBeta(alphaOrBeta, alphaBeta.beta), bestMoveEvalUpdated)
        case _                  =>
          if (alphaBeta.alpha >= alphaOrBeta) bestMoveEvalUpdated
          else alphaBetaPruningSearch(fatherNode, depth, currentPlayer, maximizingPlayer, evaluationFunc,
            allPossibleMoves.drop(1), AlphaBeta(alphaBeta.alpha, alphaOrBeta), bestMoveEvalUpdated)
      }
    }
  }
}

/**
 * It contains the necessary elements of a minimax search node.
 * @param board the board represents the current game situation
 * @param history the moves history played on the board
 */
case class MinimaxNode(board: Board, history: Seq[FullMove])

/**
 * The alpha and beta values used in the minimax alpha-beta pruning search.
 * @param alpha it is the current best move value calculated by the maximizing player
 * @param beta it is the current worst move value calculated by the minimizing player
 */
case class AlphaBeta(alpha: Double, beta: Double)