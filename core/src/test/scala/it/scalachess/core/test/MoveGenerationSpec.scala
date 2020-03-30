package it.scalachess.core.test

import it.scalachess.core.{ Black, Ongoing, White, Win }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.{ CheckValidator, MoveValidator, MovesGenerator }
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Queen }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "Applying a moveGeneration on a standard board " should "create some specific legal moves" in {
    val moveGenerator = MovesGenerator(Board.defaultBoard(), White)
    val knightsMoves = moveGenerator().filter {
      case validMove: ValidSimpleMove =>
        validMove.pieceType match {
          case Knight => true
          case _      => false
        }
    }
    knightsMoves.size should be(4)
    // println(knightsMoves)

    val pawnsMovesOnePosForward = moveGenerator().filter {
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
    pawnsMovesOnePosForward.size should be(8)
    //println(pawnsMovesOnePosForward)

    val pawnsMovesTwoPosForward = moveGenerator().filter {
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
    // println(pawnsMovesTwoPosForward)
  }

  "Test constraints of king: it" should "not being able do some move because it's under check" in {
    val whitePawnMove               = ValidSimpleMove(Pawn, White, Position(5, 2), Position(5, 4), None)
    val firstBlackPawnMove          = ValidSimpleMove(Pawn, Black, Position(5, 7), Position(5, 5), None)
    val whiteBishopMove             = ValidSimpleMove(Bishop, White, Position(6, 1), Position(4, 3), None)
    val secondBlackPawnMove         = ValidSimpleMove(Pawn, Black, Position(4, 7), Position(4, 5), None)
    val secondWhitePawnMove         = ValidSimpleMove(Pawn, White, Position(6, 2), Position(6, 4), None)
    val blackQueenMove              = ValidSimpleMove(Queen, Black, Position(4, 8), Position(8, 4), None)
    val whiteKingFromPos            = Position(5, 1)
    val whiteKingMoveNotAllowed     = ValidSimpleMove(King, White, whiteKingFromPos, Position(6, 2), None)
    val whiteKingMoveAllowed        = ValidSimpleMove(King, White, Position(5, 1), Position(5, 2), None)
    val anotherWhiteKingMoveAllowed = ValidSimpleMove(King, White, Position(5, 1), Position(6, 1), None)
    var board                       = Board.defaultBoard()
    board = board(whitePawnMove.convertInBoardMove).toOption.value
    board = board(firstBlackPawnMove.convertInBoardMove).toOption.value
    board = board(whiteBishopMove.convertInBoardMove).toOption.value
    board = board(secondBlackPawnMove.convertInBoardMove).toOption.value
    board = board(secondWhitePawnMove.convertInBoardMove).toOption.value
    CheckValidator().isKingInCheck(Black, board).toOption.value should be(false)
    board = board(blackQueenMove.convertInBoardMove).toOption.value

    CheckValidator().isKingInCheck(Black, board).toOption.value should be(true)
    val whiteKingPossibleMoves = MovesGenerator(board, White).generateAllPossiblePieceMoves(whiteKingFromPos, King)
    whiteKingPossibleMoves.contains(whiteKingMoveNotAllowed) should be(false)
    whiteKingPossibleMoves.contains(whiteKingMoveAllowed) should be(true)
    whiteKingPossibleMoves.contains(anotherWhiteKingMoveAllowed) should be(true)
  }

  /*
   * simulate a FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val firstWhitePawnPos   = Position(6, 2)
    val firstWhitePawnMove  = ValidSimpleMove(Pawn, White, firstWhitePawnPos, Position(6, 3), None)
    val blackPawnPos        = Position(5, 7)
    val blackPawnMove       = ValidSimpleMove(Pawn, Black, blackPawnPos, Position(5, 6), None)
    val secondWhitePawnPos  = Position(7, 2)
    val secondWhitePawnMove = ValidSimpleMove(Pawn, White, secondWhitePawnPos, Position(7, 4), None)
    val blackQueenPos       = Position(4, 8)
    val blackQueenMove      = ValidSimpleMove(Queen, Black, blackQueenPos, Position(8, 4), None)
    var board               = Board.defaultBoard()
    val whiteKingFromPos    = Position(5, 1)

    MovesGenerator(board, White)
      .generateAllPossiblePieceMoves(firstWhitePawnPos, Pawn)
      .contains(firstWhitePawnMove) should be(true)
    board = board(firstWhitePawnMove.convertInBoardMove).toOption.value

    MovesGenerator(board, Black)
      .generateAllPossiblePieceMoves(blackPawnPos, Pawn)
      .contains(blackPawnMove) should be(true)
    board = board(blackPawnMove.convertInBoardMove).toOption.value

    MovesGenerator(board, White)
      .generateAllPossiblePieceMoves(secondWhitePawnPos, Pawn)
      .contains(secondWhitePawnMove) should be(true)
    board = board(secondWhitePawnMove.convertInBoardMove).toOption.value

    CheckValidator().isKingInCheck(Black, board).toOption.value should be(false)

    MovesGenerator(board, Black)
      .generateAllPossiblePieceMoves(blackQueenPos, Queen)
      .contains(blackQueenMove) should be(true)
    board = board(blackQueenMove.convertInBoardMove).toOption.value

    CheckValidator().isKingInCheck(Black, board).toOption.value should be(true)
    MovesGenerator(board, White).generateAllPossiblePieceMoves(whiteKingFromPos, King).isEmpty should be(true)
    CheckValidator().isKingInCheckmate(Black, MoveValidator(board)) should be(true)
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
    val blackKingFromPos      = Position(5, 8)

    board = board(firstMoveWhitePawn.convertInBoardMove).toOption.value
    board = board(secondMoveBlackPawn.convertInBoardMove).toOption.value
    board = board(thirdMoveWhiteBishop.convertInBoardMove).toOption.value
    board = board(fourthMoveBlackKnight.convertInBoardMove).toOption.value
    board = board(fifthMoveWhiteQueen.convertInBoardMove).toOption.value
    board = board(sixthMoveBlackKnight.convertInBoardMove).toOption.value
    board = board(seventhMoveWhiteQueen.convertInBoardMove).toOption.value

    MovesGenerator(board, Black)
      .generateAllPossiblePieceMoves(blackKingFromPos, King)
      .isEmpty should be(true)

    CheckValidator().isKingInCheckmate(White, MoveValidator(board)) should be(true)
  }
}
