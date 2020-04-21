package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.generators.SimpleMoveGenerators._
import it.scalachess.core.logic.moves.{ FullMove, ValidMove, ValidPromotion, ValidSimpleMove }
import it.scalachess.core.pieces._
import it.scalachess.core.{ Black, Color, White }

private[generators] trait PieceMoves extends ((Color, Board, Position) => Seq[ValidMove]) {
  override def apply(color: Color, board: Board, from: Position): Seq[ValidMove]
}

private[generators] object PieceMoves {

  object BishopMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] = {
      val pos = Seq(1, -1)
      val result = for {
        a <- pos
        b <- pos
      } yield linearMovement(Bishop, color, board, from, Position.of(from.col + a, from.row + b), a, b)
      result.flatten
    }
  }

  object KingMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] =
      from.adjacentPositions
        .flatMap(to => simpleMove(King, color, board, from, Position.of(to)))
        .toSeq
  }

  object QueenMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] =
      (BishopMoves(color, board, from) ++ RookMoves(color, board, from))
        .map(x => ValidSimpleMove(x.from, x.to, Queen, x.color, x.capture))
  }

  object KnightMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] = {
      val pos = Seq(1, -1, 2, -2)
      val result = for {
        a <- pos
        b <- pos
        if math.abs(a) != math.abs(b)
      } yield simpleMove(Knight, color, board, from, Position.of(from.col + a, from.row + b))
      result.flatten
    }
  }

  object RookMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] = {
      val pos = Seq(1, -1, 0)
      val result = for {
        a <- pos
        b <- pos
        if math.abs(a) != math.abs(b)
      } yield linearMovement(Rook, color, board, from, Position.of(from.col + a, from.row + b), a, b)
      result.flatten
    }
  }

  object PawnMoves extends PieceMoves {
    override def apply(color: Color, board: Board, from: Position): Seq[ValidMove] =
      color match {
        case White => pawnSimpleMoves(color, board, from, from.rowUpMod, Board.whitePawnsStartingRow)
        case Black => pawnSimpleMoves(color, board, from, from.rowDownMod, Board.blackPawnsStartingRow)
      }

    def pawnSimpleMoves(color: Color,
                        board: Board,
                        from: Position,
                        forwardRowMod: Int,
                        startingRow: Int): Seq[ValidMove] = {
      val moveOnePosWithoutCapture =
        pawnMovement(color, board, from, Position.of(from.col, from.row + forwardRowMod))

      val moveTwoPosWithoutCapture =
        if (from.row == startingRow && moveOnePosWithoutCapture.isDefined)
          pawnMovement(color, board, from, Position.of(from.col, from.row + forwardRowMod * 2))
        else None

      val leftAttack =
        pawnAttack(color, board, from, Position.of(from.col + from.colLeftMod, from.row + forwardRowMod))

      val rightAttack =
        pawnAttack(color, board, from, Position.of(from.col + from.colRightMod, from.row + forwardRowMod))

      Seq(moveOnePosWithoutCapture, moveTwoPosWithoutCapture, leftAttack, rightAttack).flatten
        .flatMap(move => {
          if (isPromoting(move))
            toPromotion(move)
          else
            Seq(move)
        })
    }

    private def pawnMovement(color: Color,
                             board: Board,
                             from: Position,
                             to: Option[Position]): Option[ValidSimpleMove] =
      to match {
        case None => None // the end position doesn't exist in the board
        case Some(to) =>
          board.pieceAtPosition(to) match {
            case None    => Some(ValidSimpleMove(from, to, Pawn, color, None))
            case Some(_) => None // pawn can't pass over pieces
          }
      }

    private def pawnAttack(color: Color, board: Board, from: Position, to: Option[Position]): Option[ValidSimpleMove] =
      to match {
        case None => None // the end position doesn't exist in the board
        case Some(to) =>
          board.pieceAtPosition(to) match {
            case None => None // there's no piece to attack in the end position
            case Some(pieceToCapture) =>
              pieceToCapture.color match {
                case color.other => Some(ValidSimpleMove(from, to, Pawn, color, Some(to)))
                case _           => None // can't attack an ally piece
              }
          }
      }

    private def toPromotion(move: ValidSimpleMove): Seq[ValidPromotion] =
      Seq(Queen, Knight, Bishop, Rook)
        .map(piece => ValidPromotion(move.from, move.to, move.color, Piece(move.color, piece), Some(move.to)))

    private def isPromoting(move: ValidSimpleMove): Boolean =
      (move.color == White && move.to.row == 8) || (move.color == Black && move.to.row == 1)
  }
}

private[generators] trait PieceSpecialMoves extends ((Color, Board, Position, Seq[FullMove]) => Seq[ValidMove]) {
  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): Seq[ValidMove]
}
