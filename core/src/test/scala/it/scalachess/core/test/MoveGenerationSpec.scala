package it.scalachess.core.test

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{
  FullMove,
  KingSide,
  QueenSide,
  ValidCastling,
  ValidEnPassant,
  ValidMove,
  ValidSimpleMove
}
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "Applying a moveGeneration on a standard board " should "create some specific legal moves" in {
    val allMoves = new MoveGenerator(Board.defaultBoard(), White, Seq()).allMoves()

    val knightsMoves = allMoves.filter(_.validMove.pieceType == Knight)
    knightsMoves.size shouldBe 4

    val (pawn1TileMoves, pawn2TileMoves) = allMoves
      .filter(_.validMove.pieceType == Pawn)
      .partition { case FullMove(vm, _, _, _) => vm.from.rowDistanceAbs(vm.to) == 1 }
    pawn1TileMoves.size shouldBe 8
    pawn2TileMoves.size shouldBe 8

    val otherPieces: Set[PieceType] = Set(King, Queen, Rook, Bishop)
    val otherMoves                  = allMoves.filter(otherPieces contains _.validMove.pieceType)
    otherMoves.isEmpty shouldBe true
  }

  "Test constraints of king: it" should "not being able do some move because it's under check" in {
    val moves = Seq(
      ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None),
      ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None),
      ValidSimpleMove(Position(6, 1), Position(4, 3), Bishop, White, None),
      ValidSimpleMove(Position(4, 7), Position(4, 5), Pawn, Black, None),
      ValidSimpleMove(Position(6, 2), Position(6, 4), Pawn, White, None),
      ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    )

    val (board, player) = testMoves(moves)

    val whiteKingNotAllowed = ValidSimpleMove(Position(5, 1), Position(6, 2), King, White, None)
    val whiteKingAllowed    = ValidSimpleMove(Position(5, 1), Position(5, 2), King, White, None)
    val whiteKingAllowed_2  = ValidSimpleMove(Position(5, 1), Position(6, 1), King, White, None)

    val allMoves = new MoveGenerator(board, player, Seq()).allMoves().map(_.validMove)
    allMoves.contains(whiteKingNotAllowed) shouldBe false
    allMoves.contains(whiteKingAllowed) shouldBe true
    allMoves.contains(whiteKingAllowed_2) shouldBe true
  }

  /*
   * simulate a FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val moves = Seq(
      ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None),
      ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None),
      ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None),
    )
    val (board, player) = testMoves(moves)
    val blackQueenMate  = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)

    new MoveGenerator(board, player, Seq())
      .allMoves()
      .find(_.validMove == blackQueenMate)
      .value
      .resultsInCheckmate shouldBe true

    new MoveGenerator(board(blackQueenMate.boardChanges), player.other, Seq()).allMoves().isEmpty shouldBe true
  }
  /*
   * SCHOLAR'S MATE
   * */
  "Build a Scholar's Check Mate in which the game " should " end in 7 turn," in {
    val moves = Seq(
      ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None),
      ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None),
      ValidSimpleMove(Position(6, 1), Position(3, 4), Bishop, White, None),
      ValidSimpleMove(Position(2, 8), Position(3, 6), Knight, Black, None),
      ValidSimpleMove(Position(4, 1), Position(8, 5), Queen, White, None),
      ValidSimpleMove(Position(7, 8), Position(6, 6), Knight, Black, None)
    )

    val queenMate       = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, Some(Position(6, 7)))
    val (board, player) = testMoves(moves)

    new MoveGenerator(board, player, Seq())
      .allMoves()
      .find(_.validMove == queenMate)
      .value
      .resultsInCheckmate shouldBe true
  }

  "the first move" should "not result in a checkmate" in {
    val board = Board.defaultBoard()
    val move  = new MoveGenerator(board, White, Seq()).allMoves().headOption.value
    move.resultsInCheckmate shouldBe false
  }

  "An enpassant" should "be recognised" in {
    val moves = Seq(
      ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None),
      ValidSimpleMove(Position(1, 7), Position(1, 6), Pawn, Black, None),
      ValidSimpleMove(Position(5, 4), Position(5, 5), Pawn, White, None),
      ValidSimpleMove(Position(6, 7), Position(6, 5), Pawn, Black, None)
    )

    val blackPawnMove =
      FullMove(moves.last, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard())
    val enpassant       = ValidEnPassant(Position(5, 5), Position(6, 6), White, Position(6, 5))
    val (board, player) = testMoves(moves)
    new MoveGenerator(board, player, Seq(blackPawnMove))
      .allMoves()
      .exists(_.validMove == enpassant) shouldBe true
    val afterBoard = board(enpassant.boardChanges)
    afterBoard.pieceAtPosition(Position(6, 6)).value shouldEqual Piece(White, Pawn)
    afterBoard.pieceAtPosition(Position(6, 5)).isEmpty shouldBe true
  }

  "An QueenSide castling white" can "be adopted" in {
    val castling = ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide)
    new MoveGenerator(Board.defaultBoard(), White, Seq())
      .allMoves()
      .exists(_.validMove == castling) shouldBe false
    val moves = Seq(
      ValidSimpleMove(Position(2, 1), Position(1, 3), Knight, White, None),
      ValidSimpleMove(Position(1, 7), Position(1, 6), Pawn, Black, None),
      ValidSimpleMove(Position(2, 2), Position(2, 3), Pawn, White, None),
      ValidSimpleMove(Position(2, 7), Position(2, 6), Pawn, Black, None),
      ValidSimpleMove(Position(3, 2), Position(3, 3), Pawn, White, None),
      ValidSimpleMove(Position(3, 7), Position(3, 6), Pawn, Black, None),
      ValidSimpleMove(Position(4, 1), Position(3, 2), Queen, White, None),
      ValidSimpleMove(Position(4, 7), Position(4, 6), Pawn, Black, None),
      ValidSimpleMove(Position(3, 1), Position(2, 2), Bishop, White, None),
      ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    )

    val (board, player) = testMoves(moves)
    val fullMoves       = moves.map(FullMove(_, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard()))
    new MoveGenerator(board, player, fullMoves)
      .allMoves()
      .exists(_.validMove == castling) shouldBe true

    val secondCastlingMoves = Seq(ValidSimpleMove(Position(1, 1), Position(2, 1), Rook, White, None),
                                  ValidSimpleMove(Position(8, 7), Position(8, 6), Pawn, Black, None))

    val secondCastlingFullMoves =
      secondCastlingMoves.map(FullMove(_, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard()))

    val (afterBoard, afterPlayer) = testMoves(secondCastlingMoves, board, player)
    new MoveGenerator(afterBoard, afterPlayer, secondCastlingFullMoves)
      .allMoves()
      .exists(_.validMove == castling) shouldBe false
  }

  "An KingSide castling black" can "be adopted" in {
    val castling = ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide)
    new MoveGenerator(Board.defaultBoard(), White, Seq())
      .allMoves()
      .exists(_.validMove == castling) shouldBe false

    val moves = Seq(
      ValidSimpleMove(Position(1, 2), Position(1, 3), Pawn, White, None),
      ValidSimpleMove(Position(7, 8), Position(8, 6), Knight, Black, None),
      ValidSimpleMove(Position(2, 2), Position(2, 3), Pawn, White, None),
      ValidSimpleMove(Position(7, 7), Position(7, 6), Pawn, Black, None),
      ValidSimpleMove(Position(3, 2), Position(3, 3), Pawn, White, None),
      ValidSimpleMove(Position(6, 7), Position(6, 6), Pawn, Black, None),
      ValidSimpleMove(Position(4, 2), Position(4, 3), Pawn, White, None),
      ValidSimpleMove(Position(6, 8), Position(7, 7), Bishop, Black, None),
      ValidSimpleMove(Position(5, 2), Position(5, 3), Pawn, White, None)
    )

    val fullMoves       = moves.map(FullMove(_, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard()))
    val (board, player) = testMoves(moves)

    new MoveGenerator(board, player, fullMoves)
      .allMoves()
      .exists(_.validMove == castling) shouldBe true

    val whiteCastlingMoves = Seq(ValidSimpleMove(Position(5, 8), Position(6, 8), King, Black, None),
                                 ValidSimpleMove(Position(7, 2), Position(7, 3), Pawn, White, None))

    val whiteCastlingFullMoves =
      whiteCastlingMoves.map(FullMove(_, resultsInCheck = false, resultsInCheckmate = false, Board.defaultBoard()))

    val (afterBoard, afterPlayer) = testMoves(whiteCastlingMoves, board, player)
    new MoveGenerator(afterBoard, afterPlayer, whiteCastlingFullMoves)
      .allMoves() exists (_.validMove == castling) shouldBe false
  }

  "At the start of a game, a castling" should "not be allowed" in {
    val initialMoves = new MoveGenerator(Board.defaultBoard(), White, Seq()).allMoves().map(_.validMove)
    val castlings = Seq(
      ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide),
      ValidCastling(Position(5, 8), Position(3, 8), Black, Position(1, 8), Position(4, 8), QueenSide),
      ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide),
      ValidCastling(Position(5, 1), Position(7, 1), White, Position(8, 1), Position(6, 1), KingSide)
    )

    forAll(castlings) { castling =>
      initialMoves.contains(castling) shouldBe false
    }
  }

  private def testMoves(moves: Seq[ValidMove],
                        board: Board = Board.defaultBoard(),
                        player: Color = White): (Board, Color) =
    moves.foldLeft((board, player)) {
      case ((board, player), move) =>
        //using an empty history = Seq() for simplicity because we don't need to test it here
        new MoveGenerator(board, player, Seq())
          .allMoves()
          .exists { _.validMove == move } shouldBe true
        (board(move.boardChanges), player.other)
    }
}
