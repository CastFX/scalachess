package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.King

// TODO REFACTOR REFACTOR REFACTOR EVERYWHERE
final case class CheckValidator(board: Board, moveValidator: MoveValidator) {

  def checkOnTheEndTurn(move: ValidMove, player: Color): Boolean = {
    val boardCopy = board.copy()
    boardCopy.applyMove(move)
    val kingPos = filterKingPos(boardCopy, player)
    kingCanBeCaptured(boardCopy, kingPos, player)
  }

  def check(player: Color): Boolean = {
    val kingPos = filterKingPos(board, player)
    kingCanBeCaptured(board, kingPos, player)
  }

  def checkMate(player: Color): Boolean = {
    val kingPos                     = filterKingPos(board, player)
    val kingPiece                   = board.pieceAtPosition(kingPos).get
    val adiacentPos: List[Position] = computeKingAdiacentPos(kingPos)
    val adiacentPosThreathened: List[Position] =
      adiacentPos.filter(pos => kingCanBeCaptured(board, pos, player))
    val adiacentPosFree: List[Position] =
      adiacentPos.filter(pos =>
        if (kingPiece.canMove(kingPos, pos)) {
          moveValidator.computeShiftError(kingPos, pos, player) match {
            case Left(errorMsg)   => false
            case Right(validMove) => true
          }
        } else false)
    println(adiacentPosFree)
    println(adiacentPosThreathened)
    if (adiacentPosFree.forall(pos => adiacentPosThreathened.contains(pos))) true
    else false
  }

  private def filterKingPos(board: Board, player: Color): Position =
    board.pieces
      .filter(
        pieceEntry =>
          pieceEntry._2.color == player
          && pieceEntry._2.pieceType == King)
      .keySet
      .toList
      .head

  private def kingCanBeCaptured(board: Board, kingPos: Position, player: Color): Boolean =
    board.pieces
      .filter(pieceEntry => {
        if (pieceEntry._2.color == player.other) {
          if (pieceEntry._2.canAttack(pieceEntry._1, kingPos)) {
            moveValidator.computePathError(pieceEntry._1, kingPos, player.other) match {
              case Some(errorMsg) => false
              case None           => true
            }
          } else false
        } else false
      })
      .keySet
      .toList
      .nonEmpty

  private def computeKingAdiacentPos(kingPos: Position): List[Position] =
    kingPos.up.toList ++ kingPos.upRight.toList ++
    kingPos.right.toList ++ kingPos.downRight ++ kingPos.down ++
    kingPos.downLeft ++ kingPos.left ++ kingPos.upLeft

}
