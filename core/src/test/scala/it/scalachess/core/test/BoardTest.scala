package it.scalachess.core.test

import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.colors.{Black, White}
import it.scalachess.core.pieces.{Bishop, King, Knight, Pawn, Piece, Queen, Rook}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class BoardTest extends FlatSpec with Matchers with Inspectors {

  val initialBoard = Board.defaultBoard()
  val piecesPerType = Map(
    Pawn -> 16,
    Rook -> 4,
    Knight -> 4,
    Bishop -> 4,
    King -> 2,
    Queen -> 2
  )
  val pawnRowsNumbers = Seq(2,7)
  val majorRowsNumbers = Seq(1,8)
  val whiteRowsNumbers = Seq(1,2)
  val blackRowsNumbers = Seq(7,8)
  val pieceRowsNumbers = whiteRowsNumbers union blackRowsNumbers

  "A standard board" should
    s"have ${piecesPerType(Pawn)} pawns, " +
      s"${piecesPerType(Rook)} rooks, " +
      s"${piecesPerType(Knight)} knights, " +
      s"${piecesPerType(Bishop)} bishops, " +
      s"${piecesPerType(King)} kings, " +
      s"${piecesPerType(Queen)} queens" in {

    val piecesCounts = initialBoard.pieces
      .values
      .groupBy(identity)
      .mapValues(_.size)
      .toSet

    piecesCounts should equal (piecesPerType.toSet)
  }

  it should "have half pieces black and half pieces white" in {
    val halfPiecesPerType = piecesPerType.map { case (p, n) => (p, n/2) }.toSet

    val whitePiecesCount = initialBoard.pieces
      .values
      .filter(_.color === White)
      .groupBy(identity)
      .mapValues(_.size)
      .toSet

    halfPiecesPerType should equal (whitePiecesCount)
  }

  it should "have only pawns in row 2 and 7" in {
    val pieceTypesInRow2_7 = initialBoard.pieces
      .filter { case (pos,_) => pawnRowsNumbers.contains(pos.y) }
      .values
      .map { _.pieceType}

    all(pieceTypesInRow2_7) should be (Pawn)

  }

  it should "have row 1 and 8 as Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook" in {
    val correctRow = Seq(Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook)

    val rows_1_8 = initialBoard.pieces
      .toSeq
      .filter { case (pos, _) => majorRowsNumbers.contains(pos.y) }
      .sortBy(_._1.x)
      .partition { case (pos, _) => pos.y == 1 }

    val pieceTypesInRow1 = rows_1_8._1.map(_._2.pieceType)
    val pieceTypesInRow8 = rows_1_8._2.map(_._2.pieceType)
    pieceTypesInRow1 should equal (correctRow)
    pieceTypesInRow8 should equal (correctRow)

  }

  it should "have all white pieces in row 1-2 and black pieces in row 7-8" in {
    val whiteAndBlackPieces = initialBoard.pieces.partition { case (_,piece) => piece === White }
    val whitePieces = whiteAndBlackPieces._1.toSeq
    val blackPieces = whiteAndBlackPieces._2.toSeq

    forAll(whitePieces) { case (pos, piece) =>
      whiteRowsNumbers should contain(pos.y)
      piece.color should be (White)
    }

    forAll(blackPieces) { case (pos, piece) =>
      blackRowsNumbers should contain(pos.y)
      piece.color should be (Black)
    }
  }


  it should "have all initial positions in range [1,8]" in {
    forAll(initialBoard.pieces.keys) { pos =>
      pos.x should (be >= 1 and be <= 8)
      pos.y should (be >= 1 and be <= 8)
    }
  }

  it should "have pieces in rows 1,2,7,8" in {
    forAll(initialBoard.pieces.keys) { pos =>
      pieceRowsNumbers should contain (pos.y)
    }
  }
}
