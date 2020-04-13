package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ King, Piece }
import scalaz.Validation

case class LevelTwo() extends Level {

  private val levelOne       = LevelOne()
  private val checkmateValue = 9999.0

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] =
    moveWithMaxEvaluation(generateMovesWithEvaluation(board, aiPlayer, history, 3))

  def generateMovesWithEvaluation(board: Board,
                                  aiPlayer: Color,
                                  history: Seq[FullMove],
                                  depth: Int): Map[FullMove, Double] = {
    require(depth > 0, "The level 2 AI should compute at least the consequences of his first future move")
    val aiPlayerMoves = new MoveGenerator(board, aiPlayer, history).allMoves()
    if (isPlayerInCheckMate(board, aiPlayer.other, history)) {
      filterCheckmateMoves(aiPlayer, aiPlayerMoves, board)
    } else {
      aiPlayerMoves
        .map(fullMove => {
          fullMove -> minimax(board(fullMove.validMove.boardChanges),
                              history :+ fullMove,
                              depth - 1,
                              aiPlayer.other,
                              aiPlayer)
        })
        .toMap
    }
  }

  private def isPlayerInCheckMate(board: Board, player: Color, history: Seq[FullMove]): Boolean =
    new MoveGenerator(board, player, history).allMoves().isEmpty

  private def filterCheckmateMoves(player: Color, playerMoves: List[FullMove], board: Board): Map[FullMove, Double] =
    playerMoves
      .filter(fullMove => {
        val piecePosCaptured = fullMove.validMove.capture.getOrElse(Position(0, 0))
        board.pieceAtPosition(piecePosCaptured).getOrElse(None) == Piece(player.other, King)
      })
      .map(checkmateMoves => checkmateMoves -> -checkmateValue)
      .toMap

  def minimax(board: Board,
              history: Seq[FullMove],
              depth: Int,
              activePlayer: Color,
              aiPlayerWhichMaximize: Color): Double = {

    def recallMinimax(possibleMoves: List[FullMove],
                      board: Board,
                      history: Seq[FullMove],
                      depth: Int,
                      activePlayer: Color,
                      aiPlayerWhichMaximize: Color): List[Double] =
      possibleMoves
        .map(move => {
          minimax(board(move.validMove.boardChanges),
                  history :+ move,
                  depth - 1,
                  activePlayer.other,
                  aiPlayerWhichMaximize)
        })

    depth match {
      case 0 => levelOne.evaluateBoardByPieceValue(board, aiPlayerWhichMaximize)
      case _ =>
        val possibleMoves = new MoveGenerator(board, activePlayer, history).allMoves()
        if (possibleMoves.isEmpty) {
          activePlayer match {
            case `aiPlayerWhichMaximize` =>
              -checkmateValue // the move brings the ai player into checkmate
            case _ =>
              checkmateValue // the move brings the opposing player into checkmate
          }
        } else {
          activePlayer match {
            case `aiPlayerWhichMaximize` =>
              recallMinimax(possibleMoves, board, history, depth, activePlayer, aiPlayerWhichMaximize).max
            case _ =>
              recallMinimax(possibleMoves, board, history, depth, activePlayer, aiPlayerWhichMaximize).min
          }
        }
    }
  }

}
