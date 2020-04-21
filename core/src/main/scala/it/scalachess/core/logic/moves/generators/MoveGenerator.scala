package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.PieceWithMoveGenerator.PieceMoveGeneretorExt
import it.scalachess.core.logic.moves.{ FullMove, ValidMove }

class MoveGenerator(board: Board, player: Color, history: Seq[FullMove]) {

  /**
   * A list of valid moves, except the ones that leave the king in check.
   * @return a list of full moves.
   */
  def allMoves(): Seq[FullMove] =
    validMovesWithoutCheck()
      .map { move =>
        val boardAfter = board.apply(move.boardChanges)
        val check      = resultsInCheck(move, player.other)
        val checkMate  = check && resultsInCheckmate(move, player.other)
        FullMove(move, check, checkMate, boardAfter)
      }
      .filter(move => !resultsInCheck(move.validMove, player))

  private def validMovesWithoutCheck(): Seq[ValidMove] =
    board.pieces
      .filter(_._2.color == player)
      .flatMap { case (pos, piece) => piece.validMoves(pos, board, history) }
      .toSeq

  private def resultsInCheck(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard = board.apply(move.boardChanges)
    new MoveGenerator(afterBoard, kingColor.other, history)
      .validMovesWithoutCheck()
      .exists(capturesKing(_, kingColor, afterBoard))
  }

  private def resultsInCheckmate(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard     = board.apply(move.boardChanges)
    val afterGenerator = new MoveGenerator(afterBoard, kingColor, history)
    afterGenerator
      .validMovesWithoutCheck()
      .forall(afterGenerator.resultsInCheck(_, kingColor))
  }

  private def capturesKing(move: ValidMove, kingColor: Color, board: Board): Boolean =
    board
      .apply(move.boardChanges)
      .kingPos(kingColor)
      .isEmpty
}
