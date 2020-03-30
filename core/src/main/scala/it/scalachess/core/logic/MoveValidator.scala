package it.scalachess.core.logic

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ BoardMove, ParsedMove, ValidMove }
import scalaz.{ Failure, Success, Validation }
//import it.scalachess.core.logic.moves.Move
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }

final case class MoveValidator(board: Board) {

  //def apply(move: Move, player: Color): Unit = ???

  def canAttack() = {}

  def pieceCanAttack(pieceType: PieceType, color: Color, start: Position, end: Position): Boolean = {
    def knightCanAttack(): Boolean = {
      val rowDistance = start rowDistanceAbs end
      val colDistance = start colDistanceAbs end
      (rowDistance == 2 && colDistance == 1
      || rowDistance == 1 && colDistance == 2)
    }
    def pawnCanAttack(): Boolean = {
      val rowDistance = end rowDistance start
      val colDistance = start colDistanceAbs end
      (color == White && rowDistance == 1 && colDistance == 1
      || color == Black && rowDistance == -1 && colDistance == 1)
    }
    pieceType match {
      case King   => start isAdjacentTo end
      case Queen  => (start isDiagonalTo end) || (start isStraightTo end)
      case Rook   => start isStraightTo end
      case Bishop => start isDiagonalTo end
      case Knight => knightCanAttack()
      case Pawn   => pawnCanAttack()
    }
  }

  /**
   * Checks if the path crossed by the piece moved is free of pieces.
   * This method assume that the correctness of the two end point positions is already checked.
   * @param from the start position where is located the (active) player's piece
   * @param to   the final position where the piece should move
   * @return Option containing the error message
   */
  def generatePathError(from: Position, to: Position): Option[String] = {

    def generateErrorPieceInPath(path: Set[Position]) =
      if (path.forall(p => board.pieceAtPosition(p).isEmpty)) None
      else Some("The piece can't move throught other pieces")

    def generateErrorInStraightPath(from: Position, to: Position) =
      if (from.colDistanceAbs(to) == 0)
        generateErrorPieceInPath(from.generatePosBetweenRow(to, Set()))
      else
        generateErrorPieceInPath(from.generatePosBetweenCol(to, Set()))

    def generateErrorInDiagonalPath(from: Position, to: Position) =
      generateErrorPieceInPath(from.generatePosBetweenDiagonal(to, Set()))

    board.pieceAtPosition(from) map (piece => piece.pieceType) getOrElse None match {
      case Rook   => generateErrorInStraightPath(from, to)
      case Bishop => generateErrorInDiagonalPath(from, to)
      case Queen =>
        if (from.colDistanceAbs(to) == 0 || from.rowDistanceAbs(to) == 0)
          generateErrorInStraightPath(from, to)
        else
          generateErrorInDiagonalPath(from, to)
      case Pawn =>
        if (from.colDistance(to) == 1)
          generateErrorInStraightPath(from, to)
        else None
      case _ => None
    }
  }

  def validateParsedMove(move: ParsedMove, player: Color): Validation[String, BoardMove] = {
    val errorMessage: String        = "The move is not a valid one"
    val ambiguityMessage: String    = "This move creates an ambiguity, please specify it better"
    val validMoves: List[ValidMove] = MovesGenerator(board, player)()
    val parsedMoves: List[ParsedMove] = validMoves
      .map(validMove => validMove.convertInParsedMove(board))
    val map: Map[ValidMove, ParsedMove] = (validMoves zip parsedMoves).toMap
    val filteredMap = map.filter(parsed => move.isEqualTo(parsed._2))
    filteredMap.size match {
      case 0 => Failure(errorMessage)
      case 1 => {
        Success(filteredMap.head._1.convertInBoardMove)
      }
      case _ => Failure(ambiguityMessage)
    }
  }
}
