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
import scalaz.Validation

private case class Check(check: Boolean, checkmate: Boolean)

trait NonAmbiguous extends Parser[AlgebraicMove, String] with PGNFormatter[String] {
  abstract override def parse(t: AlgebraicMove): Validation[String, String] =
    super.parse(t)

  /**
   * Parse the input into a string representing the game
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
          if (capture.isDefined) Some((from.col + 96).toChar) else None,
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
                            Some((from.col + 96).toChar),
                            None,
                            None)
    }
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
        AlgebraicSimpleMove(to, pieceType, capture.isDefined, resultsInCheck, resultsInCheckmate, None, None, None)
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
                              Some((from.col + 96).toChar),
                              None,
                              None)
        }
      case 3 =>
        AlgebraicSimpleMove(to,
                            pieceType,
                            capture.isDefined,
                            resultsInCheck,
                            resultsInCheckmate,
                            Some((from.col + 96).toChar),
                            Some(from.row),
                            None)
    }
  }
}
