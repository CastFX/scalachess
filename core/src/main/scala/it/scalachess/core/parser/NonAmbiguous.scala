package it.scalachess.core.parser

import it.scalachess.core.Result
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.generators.PieceGenerators
import it.scalachess.core.logic.moves.{
  AlgebraicCastling,
  AlgebraicMove,
  AlgebraicSimpleMove,
  FullMove,
  ValidCastling,
  ValidEnPassant,
  ValidMove,
  ValidPromotion,
  ValidSimpleMove
}
import it.scalachess.core.parser.Parser.Parser
import it.scalachess.core.pieces.Pawn
import scalaz.Validation
import it.scalachess.core.board.Position.colToChar

/**
 * Mixin for a parser and with a PGNFormatter.
 * It uses the parent parse and parseAll methods.
 * It add the possibility of converting a List of FullMove into AlgebraicMoves.
 */
trait NonAmbiguous extends Parser[AlgebraicMove, String] with PGNFormatter[String] {
  abstract override def parse(t: AlgebraicMove): Validation[String, String] =
    super.parse(t)

  /**
   * Convert a list of FullMoves into disambiguate AlgebraicMove.
   * Parse the AlgebraicMoves with the father parser and format with the PGNFormatter.
   * @param seq the input to be parsed
   * @return a representation of the game
   */
  def convertAndFormat(seq: Seq[FullMove], result: Option[Result]): String = {
    val validMoves = seq.map { full =>
      full.validMove -> Check(full.resultsInCheck, full.resultsInCheckmate)
    }
    val history = validMoves.map {
      case (move: ValidMove, check: Check) =>
        val movesBefore = validMoves.span(_ != (move, check))._1
        val boardBefore =
          movesBefore.foldLeft(Board.defaultBoard())((board, move) => board(move._1.boardChanges))
        FullMove(move, resultsInCheck = check.check, resultsInCheckmate = check.checkmate, boardBefore)
    }

    val moves: Seq[AlgebraicMove] = history.map(disambiguate)
    format(parseAll(moves), result)
  }

  /**
   * Disambiguate a FullMove by converting it in an AlgebraicMove that refers to a single possible move on the board.
   * @param move the move to be disambiguated
   * @return the move without ambiguity
   */
  private def disambiguate(move: FullMove): AlgebraicMove =
    move.validMove match {
      case ValidCastling(_, _, _, _, _, castling) =>
        AlgebraicCastling(castling, move.resultsInCheck, move.resultsInCheckmate)
      case simple: ValidSimpleMove =>
        validSimpleDisambiguation(move, simple)
      case promotion: ValidPromotion =>
        import promotion._
        import move._
        AlgebraicSimpleMove(
          to,
          pieceType,
          capture.isDefined,
          resultsInCheck,
          resultsInCheckmate,
          if (capture.isDefined) Some(colToChar(from.col)) else None,
          None,
          Some(promotesTo.pieceType)
        )
      case enpassant: ValidEnPassant =>
        import enpassant._
        import move._
        AlgebraicSimpleMove(to,
                            pieceType,
                            capture.isDefined,
                            resultsInCheck,
                            resultsInCheckmate,
                            Some(colToChar(from.col)),
                            None,
                            None)
    }

  /**
   * Disambiguate a simple move and return the relative Algebraic Move.
   * @param move the full move to be disambiguated
   * @param simple the simple move to be disambiguated
   * @return an AlgebraicSimpleMove without ambiguity
   */
  private def validSimpleDisambiguation(move: FullMove, simple: ValidSimpleMove): AlgebraicSimpleMove = {
    import simple._
    import move._
    val moves = boardAfter.pieces.flatMap {
      case (pos, piece) =>
        PieceGenerators
          .PieceWithMoveGenerator(piece)
          .pieceSimpleValidMoves(pos, boardAfter)
          .filter(x => x.to == to && x.pieceType == pieceType && x.color == color)
    }
    moves.size match {
      case 1 =>
        AlgebraicSimpleMove(to,
                            pieceType,
                            capture.isDefined,
                            resultsInCheck,
                            resultsInCheckmate,
                            if (capture.isDefined && pieceType == Pawn) Some(colToChar(from.col)) else None,
                            None,
                            None)
      case 2 =>
        if (moves.head.from.col == moves.last.from.col) {
          AlgebraicSimpleMove(to,
                              pieceType,
                              capture.isDefined,
                              resultsInCheck,
                              resultsInCheckmate,
                              None,
                              Some(from.row),
                              None)
        } else {
          AlgebraicSimpleMove(to,
                              pieceType,
                              capture.isDefined,
                              resultsInCheck,
                              resultsInCheckmate,
                              Some(colToChar(from.col)),
                              None,
                              None)
        }
      case 3 =>
        AlgebraicSimpleMove(to,
                            pieceType,
                            capture.isDefined,
                            resultsInCheck,
                            resultsInCheckmate,
                            Some(colToChar(from.col)),
                            Some(from.row),
                            None)
    }
  }
  private case class Check(check: Boolean, checkmate: Boolean)
}
