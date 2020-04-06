package it.scalachess.core.test

import it.scalachess.core.{ Black, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, KingSide, QueenSide, ValidCastling, ValidEnPassant, ValidSimpleMove }
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen, Rook }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "Applying a moveGeneration on a standard board " should "create some specific legal moves" in {
    val moveGenerator = new MoveGenerator(Board.defaultBoard(), White, Seq())
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

    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(_1_whitePawnMove) should be(true)
    board = board(_1_whitePawnMove.boardChanges)

    new MoveGenerator(board, Black, Seq()).allMoves().map(_.validMove).contains(_2_firstBlackPawnMove) should be(true)
    board = board(_2_firstBlackPawnMove.boardChanges)

    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(_3_whiteBishopMove) should be(true)
    board = board(_3_whiteBishopMove.boardChanges)

    new MoveGenerator(board, Black, Seq()).allMoves().map(_.validMove).contains(_4_secondBlackPawnMove) should be(true)
    board = board(_4_secondBlackPawnMove.boardChanges)

    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(_5_secondWhitePawnMove) should be(true)
    board = board(_5_secondWhitePawnMove.boardChanges)

    val moves = new MoveGenerator(board, Black, Seq()).allMoves()
    moves.map(_.validMove).contains(_6_blackQueenMove) should be(true)
    board = board(_6_blackQueenMove.boardChanges)

    moves.find(_.validMove equals _6_blackQueenMove).value.resultsInCheck shouldBe true

    val whiteKingPossibleMoves = new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove)
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

    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(firstWhitePawnMove) should be(true)
    board = board(firstWhitePawnMove.boardChanges)

    new MoveGenerator(board, Black, Seq()).allMoves().map(_.validMove).contains(blackPawnMove) should be(true)
    board = board(blackPawnMove.boardChanges)

    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(secondWhitePawnMove) should be(true)
    board = board(secondWhitePawnMove.boardChanges)

    val moves = new MoveGenerator(board, Black, Seq()).allMoves()
    moves.map(_.validMove).contains(blackQueenMove) should be(true)
    board = board(blackQueenMove.boardChanges)
    moves.find(_.validMove equals blackQueenMove).value.resultsInCheckmate shouldBe true

    new MoveGenerator(board, White, Seq()).allMoves().isEmpty should be(true)
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
    val seventhMoveWhiteQueen = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, Some(Position(6, 7)))
    var board                 = Board.defaultBoard()

    board = board(firstMoveWhitePawn.boardChanges)
    board = board(secondMoveBlackPawn.boardChanges)
    board = board(thirdMoveWhiteBishop.boardChanges)
    board = board(fourthMoveBlackKnight.boardChanges)
    board = board(fifthMoveWhiteQueen.boardChanges)
    board = board(sixthMoveBlackKnight.boardChanges)

    val moves = new MoveGenerator(board, White, Seq()).allMoves()
    moves.map(_.validMove).contains(seventhMoveWhiteQueen) shouldBe true
    board = board(seventhMoveWhiteQueen.boardChanges)

    moves.find(_.validMove equals seventhMoveWhiteQueen).value.resultsInCheckmate shouldBe true
  }

  "the first move" should "not result in a checkmate" in {
    val board = Board.defaultBoard()
    val move  = new MoveGenerator(board, White, Seq()).allMoves().headOption.value
    move.resultsInCheckmate shouldBe false
  }

  "An en passant" should "be recognised" in {
    val firstMoveWhitePawn        = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
    val secondMoveBlackPawn       = ValidSimpleMove(Position(1, 7), Position(1, 6), Pawn, Black, None)
    val thirdMoveWhitePawn        = ValidSimpleMove(Position(5, 4), Position(5, 5), Pawn, White, None)
    val blackPawnTriggerEnPassant = ValidSimpleMove(Position(6, 7), Position(6, 5), Pawn, Black, None)
    val blackPawnTriggerFullMove =
      FullMove(blackPawnTriggerEnPassant, resultsInCheck = false, resultsInCheckmate = false)
    val shouldEnPassant = ValidEnPassant(Position(5, 5), Position(6, 6), White, Position(6, 5))
    val moves           = Seq(firstMoveWhitePawn, secondMoveBlackPawn, thirdMoveWhitePawn, blackPawnTriggerEnPassant)
    var board: Board    = Board.defaultBoard()
    moves.foreach(move => board = board(move.boardChanges))
    val validMoves = new MoveGenerator(board, White, Seq(blackPawnTriggerFullMove)).allMoves()
    validMoves.map(_.validMove).contains(shouldEnPassant) shouldBe true
    board = board(shouldEnPassant.boardChanges)
    board.pieceAtPosition(Position(6, 6)).value shouldEqual Piece(White, Pawn)
    board.pieceAtPosition(Position(6, 5)).isEmpty shouldBe true
  }

  "An QueenSide castling white" can "be adopted" in {
    var board: Board = Board.defaultBoard()
    val castling     = ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide)
    val validMoves   = new MoveGenerator(board, White, Seq()).allMoves()
    validMoves.map(_.validMove).contains(castling) shouldBe false
    val firstMove   = ValidSimpleMove(Position(2, 1), Position(1, 3), Knight, White, None)
    val secondMove  = ValidSimpleMove(Position(1, 7), Position(1, 6), Pawn, Black, None)
    val thirdMove   = ValidSimpleMove(Position(2, 2), Position(2, 3), Pawn, White, None)
    val fourthMove  = ValidSimpleMove(Position(2, 7), Position(2, 6), Pawn, Black, None)
    val fifthMove   = ValidSimpleMove(Position(3, 2), Position(3, 3), Pawn, White, None)
    val sixthMove   = ValidSimpleMove(Position(3, 7), Position(3, 6), Pawn, Black, None)
    val seventhMove = ValidSimpleMove(Position(4, 1), Position(3, 2), Queen, White, None)
    val eighthMove  = ValidSimpleMove(Position(4, 7), Position(4, 6), Pawn, Black, None)
    val ninthMove   = ValidSimpleMove(Position(3, 1), Position(2, 2), Bishop, White, None)
    val tenthMove   = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val fullMove    = FullMove(tenthMove, resultsInCheck = false, resultsInCheckmate = false)
    val moves = Seq(firstMove,
                    secondMove,
                    thirdMove,
                    fourthMove,
                    fifthMove,
                    sixthMove,
                    seventhMove,
                    eighthMove,
                    ninthMove,
                    tenthMove)
    moves.foreach(move => board = board(move.boardChanges))
    val move = new MoveGenerator(board, White, Seq(fullMove)).allMoves()
    move.map(_.validMove).contains(castling) shouldBe true
    val rookMove      = ValidSimpleMove(Position(1, 1), Position(2, 1), Rook, White, None)
    val blackMove     = ValidSimpleMove(Position(8, 7), Position(8, 6), Pawn, Black, None)
    val fullMoveBlack = FullMove(blackMove, resultsInCheck = false, resultsInCheckmate = false)
    Seq(rookMove, blackMove).foreach(move => board = board(move.boardChanges))
    new MoveGenerator(board, White, Seq(fullMoveBlack)).allMoves().map(_.validMove).contains(castling) shouldBe false
  }

  "An KingSide castling black" can "be adopted" in {
    var board: Board = Board.defaultBoard()
    val castling     = ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide)
    val validMoves   = new MoveGenerator(board, White, Seq()).allMoves()
    validMoves.map(_.validMove).contains(castling) shouldBe false
    val firstMove   = ValidSimpleMove(Position(1, 2), Position(1, 3), Pawn, White, None)
    val secondMove  = ValidSimpleMove(Position(7, 8), Position(8, 6), Knight, Black, None)
    val thirdMove   = ValidSimpleMove(Position(2, 2), Position(2, 3), Pawn, White, None)
    val fourthMove  = ValidSimpleMove(Position(7, 7), Position(7, 6), Pawn, Black, None)
    val fifthMove   = ValidSimpleMove(Position(3, 2), Position(3, 3), Pawn, White, None)
    val sixthMove   = ValidSimpleMove(Position(6, 7), Position(6, 6), Pawn, Black, None)
    val seventhMove = ValidSimpleMove(Position(4, 2), Position(4, 3), Pawn, White, None)
    val eighthMove  = ValidSimpleMove(Position(6, 8), Position(7, 7), Bishop, Black, None)
    val ninthMove   = ValidSimpleMove(Position(5, 2), Position(5, 3), Pawn, White, None)
    val fullMove    = FullMove(ninthMove, resultsInCheck = false, resultsInCheckmate = false)
    val moves =
      Seq(firstMove, secondMove, thirdMove, fourthMove, fifthMove, sixthMove, seventhMove, eighthMove, ninthMove)
    moves.foreach(move => board = board(move.boardChanges))
    val move = new MoveGenerator(board, Black, Seq(fullMove)).allMoves()
    move.map(_.validMove).contains(castling) shouldBe true

    val kingMove      = ValidSimpleMove(Position(5, 8), Position(6, 8), King, Black, None)
    val whiteMove     = ValidSimpleMove(Position(7, 2), Position(7, 3), Pawn, Black, None)
    val fullMoveWhite = FullMove(whiteMove, resultsInCheck = false, resultsInCheckmate = false)
    Seq(kingMove, whiteMove).foreach(move => board = board(move.boardChanges))
    new MoveGenerator(board, Black, Seq(fullMoveWhite)).allMoves().map(_.validMove).contains(castling) shouldBe false
  }

  "At the start of a game, a castling" should "not be allowed" in {
    var board: Board = Board.defaultBoard()
    val castlingBlackKing =
      ValidCastling(Position(5, 8), Position(7, 8), Black, Position(8, 8), Position(6, 8), KingSide)
    val castlingBlackQueen =
      ValidCastling(Position(5, 8), Position(3, 8), Black, Position(1, 8), Position(4, 8), QueenSide)
    val castlingWhiteQueen =
      ValidCastling(Position(5, 1), Position(3, 1), White, Position(1, 1), Position(4, 1), QueenSide)
    val castlingWhiteKing =
      ValidCastling(Position(5, 1), Position(7, 1), White, Position(8, 1), Position(6, 1), KingSide)
    new MoveGenerator(board, Black, Seq()).allMoves().map(_.validMove).contains(castlingBlackKing) shouldBe false
    new MoveGenerator(board, Black, Seq()).allMoves().map(_.validMove).contains(castlingBlackQueen) shouldBe false
    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(castlingWhiteKing) shouldBe false
    new MoveGenerator(board, White, Seq()).allMoves().map(_.validMove).contains(castlingWhiteQueen) shouldBe false
  }
}
