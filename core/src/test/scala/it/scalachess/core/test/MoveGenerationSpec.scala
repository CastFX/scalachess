package it.scalachess.core.test

import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ IsKingInCheck, IsKingInCheckmate }
import it.scalachess.core.logic.moves.{ generators, ValidSimpleMove }
import it.scalachess.core.logic.moves.generators.MovesGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Queen }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "Applying a moveGeneration on a standard board " should "create some specific legal moves" in {
    val moveGenerator = MovesGenerator(White, Board.defaultBoard())
    val knightsMoves = moveGenerator(true).filter {
      case validMove: ValidSimpleMove =>
        validMove.pieceType match {
          case Knight => true
          case _      => false
        }
    }
    knightsMoves.size should be(4)

    val pawnsMovesOnePosForward = moveGenerator(true).filter {
      case validMove: ValidSimpleMove =>
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

    val pawnsMovesTwoPosForward = moveGenerator(true).filter {
      case validMove: ValidSimpleMove =>
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

    val kingMoves = moveGenerator(true).filter {
      case validMove: ValidSimpleMove =>
        validMove.pieceType match {
          case King => true
          case _    => false
        }
    }
    kingMoves.isEmpty should be(true)

    val queenMoves = moveGenerator(true).filter {
      case validMove: ValidSimpleMove =>
        validMove.pieceType match {
          case King => true
          case _    => false
        }
    }
    queenMoves.isEmpty should be(true)

  }

  "Test constraints of king: it" should "not being able do some move because it's under check" in {
    // '_n_' located in the val names means the order in which execute the move
    val _1_whitePawnMove               = ValidSimpleMove(Pawn, White, Position(5, 2), Position(5, 4), None)
    val _2_firstBlackPawnMove          = ValidSimpleMove(Pawn, Black, Position(5, 7), Position(5, 5), None)
    val _3_whiteBishopMove             = ValidSimpleMove(Bishop, White, Position(6, 1), Position(4, 3), None)
    val _4_secondBlackPawnMove         = ValidSimpleMove(Pawn, Black, Position(4, 7), Position(4, 5), None)
    val _5_secondWhitePawnMove         = ValidSimpleMove(Pawn, White, Position(6, 2), Position(6, 4), None)
    val _6_blackQueenMove              = ValidSimpleMove(Queen, Black, Position(4, 8), Position(8, 4), None)
    val _7_whiteKingMoveNotAllowed     = ValidSimpleMove(King, White, Position(5, 1), Position(6, 2), None)
    val _7_whiteKingMoveAllowed        = ValidSimpleMove(King, White, Position(5, 1), Position(5, 2), None)
    val _7_anotherWhiteKingMoveAllowed = ValidSimpleMove(King, White, Position(5, 1), Position(6, 1), None)
    val exposedToKingInCheck           = true
    var board                          = Board.defaultBoard()

    generators.MovesGenerator(White, board)(exposedToKingInCheck).contains(_1_whitePawnMove) should be(true)
    board = board(_1_whitePawnMove.convertInBoardMove)

    generators.MovesGenerator(Black, board)(exposedToKingInCheck).contains(_2_firstBlackPawnMove) should be(true)
    board = board(_2_firstBlackPawnMove.convertInBoardMove)

    generators.MovesGenerator(White, board)(exposedToKingInCheck).contains(_3_whiteBishopMove) should be(true)
    board = board(_3_whiteBishopMove.convertInBoardMove)

    generators.MovesGenerator(Black, board)(exposedToKingInCheck).contains(_4_secondBlackPawnMove) should be(true)
    board = board(_4_secondBlackPawnMove.convertInBoardMove)

    generators.MovesGenerator(White, board)(exposedToKingInCheck).contains(_5_secondWhitePawnMove) should be(true)
    board = board(_5_secondWhitePawnMove.convertInBoardMove)

    generators.MovesGenerator(Black, board)(exposedToKingInCheck).contains(_6_blackQueenMove) should be(true)
    board = board(_6_blackQueenMove.convertInBoardMove)

    IsKingInCheck(Black, board) should be(false)
    IsKingInCheck(White, board) should be(true)
    val whiteKingPossibleMoves = generators.MovesGenerator(White, board)(exposedToKingInCheck)
    whiteKingPossibleMoves.contains(_7_whiteKingMoveNotAllowed) should be(false)
    whiteKingPossibleMoves.contains(_7_whiteKingMoveAllowed) should be(true)
    whiteKingPossibleMoves.contains(_7_anotherWhiteKingMoveAllowed) should be(true)
  }

  /*
   * simulate a FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val firstWhitePawnMove   = ValidSimpleMove(Pawn, White, Position(6, 2), Position(6, 3), None)
    val blackPawnMove        = ValidSimpleMove(Pawn, Black, Position(5, 7), Position(5, 6), None)
    val secondWhitePawnMove  = ValidSimpleMove(Pawn, White, Position(7, 2), Position(7, 4), None)
    val blackQueenMove       = ValidSimpleMove(Queen, Black, Position(4, 8), Position(8, 4), None)
    val exposedToKingInCheck = true
    var board                = Board.defaultBoard()

    generators.MovesGenerator(White, board)(exposedToKingInCheck).contains(firstWhitePawnMove) should be(true)
    board = board(firstWhitePawnMove.convertInBoardMove)

    generators.MovesGenerator(Black, board)(exposedToKingInCheck).contains(blackPawnMove) should be(true)
    board = board(blackPawnMove.convertInBoardMove)

    generators.MovesGenerator(White, board)(exposedToKingInCheck).contains(secondWhitePawnMove) should be(true)
    board = board(secondWhitePawnMove.convertInBoardMove)

    generators.MovesGenerator(Black, board)(exposedToKingInCheck).contains(blackQueenMove) should be(true)
    board = board(blackQueenMove.convertInBoardMove)

    IsKingInCheck(Black, board) should be(false)
    IsKingInCheck(White, board) should be(true)
    generators.MovesGenerator(White, board)(exposedToKingInCheck).isEmpty should be(true)
    IsKingInCheckmate(White, board) should be(true)
  }
  /*
   * SCHOLAR'S MATE
   * */
  "Build a Scholar's Check Mate in which the game " should " end in 7 turn," in {
    val firstMoveWhitePawn    = ValidSimpleMove(Pawn, White, Position(5, 2), Position(5, 4), None)
    val secondMoveBlackPawn   = ValidSimpleMove(Pawn, Black, Position(5, 7), Position(5, 5), None)
    val thirdMoveWhiteBishop  = ValidSimpleMove(Bishop, White, Position(6, 1), Position(3, 4), None)
    val fourthMoveBlackKnight = ValidSimpleMove(Knight, Black, Position(2, 8), Position(3, 6), None)
    val fifthMoveWhiteQueen   = ValidSimpleMove(Queen, White, Position(4, 1), Position(8, 5), None)
    val sixthMoveBlackKnight  = ValidSimpleMove(Knight, Black, Position(7, 8), Position(6, 6), None)
    val seventhMoveWhiteQueen = ValidSimpleMove(Queen, White, Position(8, 5), Position(6, 7), None)
    var board                 = Board.defaultBoard()

    board = board(firstMoveWhitePawn.convertInBoardMove)
    board = board(secondMoveBlackPawn.convertInBoardMove)
    board = board(thirdMoveWhiteBishop.convertInBoardMove)
    board = board(fourthMoveBlackKnight.convertInBoardMove)
    board = board(fifthMoveWhiteQueen.convertInBoardMove)
    board = board(sixthMoveBlackKnight.convertInBoardMove)
    board = board(seventhMoveWhiteQueen.convertInBoardMove)

    IsKingInCheckmate(Black, board) should be(true)
  }
}
