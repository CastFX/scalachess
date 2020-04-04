package it.scalachess.core.test

import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidSimpleMove }
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.logic.moves.generators.PieceGenerators.PieceWithMoveGenerator

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "Applying a moveGeneration on a standard board " should "create some specific legal moves" in {
    val moveGenerator = new MoveGenerator(Board.defaultBoard(), White)
    val knightsMoves = moveGenerator.allMoves().filter {
      case FullMove(validMove, _, _) =>
        validMove.pieceType match {
          case Knight => true
          case _      => false
        }
    }
    knightsMoves.size should be(4)

    val pawnsMovesOnePosForward = moveGenerator.allMoves().filter {
      case FullMove(validMove, _, _) =>
        validMove.pieceType match {
          case Pawn =>
            validMove.to rowDistanceAbs validMove.from match {
              case 1 => true
              case _ => false
            }
          case _ => false
        }
    }
    pawnsMovesOnePosForward.size should be(8)

    val pawnsMovesTwoPosForward = moveGenerator.allMoves().filter {
      case FullMove(validMove, _, _) =>
        validMove.pieceType match {
          case Pawn =>
            validMove.to rowDistanceAbs validMove.from match {
              case 2 => true
              case _ => false
            }
          case _ => false
        }
    }
    pawnsMovesTwoPosForward.size should be(8)

    val kingMoves = moveGenerator.allMoves().filter {
      case FullMove(validMove, _, _) =>
        validMove.pieceType match {
          case King => true
          case _    => false
        }
    }
    kingMoves.isEmpty should be(true)

    val queenMoves = moveGenerator.allMoves().filter {
      case FullMove(validMove, _, _) =>
        validMove.pieceType match {
          case King => true
          case _    => false
        }
    }
    queenMoves.isEmpty should be(true)

  }

  "Test constraints of king: it" should "not being able do some move because it's under check" in {
    // '_n_' located in the val names means the order in which execute the move
    val _1_whitePawnMove               = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
    val _2_firstBlackPawnMove          = ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None)
    val _3_whiteBishopMove             = ValidSimpleMove(Position(6, 1), Position(4, 3), Bishop, White, None)
    val _4_secondBlackPawnMove         = ValidSimpleMove(Position(4, 7), Position(4, 5), Pawn, Black, None)
    val _5_secondWhitePawnMove         = ValidSimpleMove(Position(6, 2), Position(6, 4), Pawn, White, None)
    val _6_blackQueenMove              = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    val _7_whiteKingMoveNotAllowed     = ValidSimpleMove(Position(5, 1), Position(6, 2), King, White, None)
    val _7_whiteKingMoveAllowed        = ValidSimpleMove(Position(5, 1), Position(5, 2), King, White, None)
    val _7_anotherWhiteKingMoveAllowed = ValidSimpleMove(Position(5, 1), Position(6, 1), King, White, None)
    var board                          = Board.defaultBoard()

    new MoveGenerator(board, White).allMoves().map(_.validMove).contains(_1_whitePawnMove) should be(true)
    board = board(_1_whitePawnMove.boardChanges)

    new MoveGenerator(board, Black).allMoves().map(_.validMove).contains(_2_firstBlackPawnMove) should be(true)
    board = board(_2_firstBlackPawnMove.boardChanges)

    new MoveGenerator(board, White).allMoves().map(_.validMove).contains(_3_whiteBishopMove) should be(true)
    board = board(_3_whiteBishopMove.boardChanges)

    new MoveGenerator(board, Black).allMoves().map(_.validMove).contains(_4_secondBlackPawnMove) should be(true)
    board = board(_4_secondBlackPawnMove.boardChanges)

    new MoveGenerator(board, White).allMoves().map(_.validMove).contains(_5_secondWhitePawnMove) should be(true)
    board = board(_5_secondWhitePawnMove.boardChanges)

    val moves = new MoveGenerator(board, Black).allMoves()
    moves.map(_.validMove).contains(_6_blackQueenMove) should be(true)
    board = board(_6_blackQueenMove.boardChanges)

    moves.find(_.validMove equals _6_blackQueenMove).value.resultsInCheck shouldBe true

    val whiteKingPossibleMoves = new MoveGenerator(board, White).allMoves().map(_.validMove)
    whiteKingPossibleMoves.contains(_7_whiteKingMoveNotAllowed) should be(false)
    whiteKingPossibleMoves.contains(_7_whiteKingMoveAllowed) should be(true)
    whiteKingPossibleMoves.contains(_7_anotherWhiteKingMoveAllowed) should be(true)
  }

  /*
   * simulate a FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val firstWhitePawnMove  = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
    val blackPawnMove       = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
    val blackQueenMove      = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    var board               = Board.defaultBoard()

    new MoveGenerator(board, White).allMoves().map(_.validMove).contains(firstWhitePawnMove) should be(true)
    board = board(firstWhitePawnMove.boardChanges)

    new MoveGenerator(board, Black).allMoves().map(_.validMove).contains(blackPawnMove) should be(true)
    board = board(blackPawnMove.boardChanges)

    new MoveGenerator(board, White).allMoves().map(_.validMove).contains(secondWhitePawnMove) should be(true)
    board = board(secondWhitePawnMove.boardChanges)

    val moves = new MoveGenerator(board, Black).allMoves()
    moves.map(_.validMove).contains(blackQueenMove) should be(true)
    board = board(blackQueenMove.boardChanges)
    println(board)
    println(moves.find(_.validMove equals blackQueenMove))
    moves.find(_.validMove equals blackQueenMove).value.resultsInCheckmate shouldBe true

    new MoveGenerator(board, White).allMoves().isEmpty should be(true)
  }
  /*
   * SCHOLAR'S MATE
   * */
  "Build a Scholar's Check Mate in which the game " should " end in 7 turn," in {
    val firstMoveWhitePawn    = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
    val secondMoveBlackPawn   = ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None)
    val thirdMoveWhiteBishop  = ValidSimpleMove(Position(6, 1), Position(3, 4), Bishop, White, None)
    val fourthMoveBlackKnight = ValidSimpleMove(Position(2, 8), Position(3, 6), Knight, Black, None)
    val fifthMoveWhiteQueen   = ValidSimpleMove(Position(4, 1), Position(8, 5), Queen, White, None)
    val sixthMoveBlackKnight  = ValidSimpleMove(Position(7, 8), Position(6, 6), Knight, Black, None)
    val seventhMoveWhiteQueen = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, None)
    var board                 = Board.defaultBoard()

    board = board(firstMoveWhitePawn.boardChanges)
    board = board(secondMoveBlackPawn.boardChanges)
    board = board(thirdMoveWhiteBishop.boardChanges)
    board = board(fourthMoveBlackKnight.boardChanges)
    board = board(fifthMoveWhiteQueen.boardChanges)
    board = board(sixthMoveBlackKnight.boardChanges)

    val moves = new MoveGenerator(board, Black).allMoves()
    moves.map(_.validMove).contains(seventhMoveWhiteQueen) should be(true)
    board = board(seventhMoveWhiteQueen.boardChanges)

    moves.find(_.validMove equals seventhMoveWhiteQueen).value.resultsInCheckmate shouldBe true
  }

  "the first move" should "not result in a checkmate" in {
    val board = Board.defaultBoard()
    val move  = new MoveGenerator(board, White).allMoves().headOption.value
    move.resultsInCheckmate shouldBe false
  }
}
