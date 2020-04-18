package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.{FullMove, ValidSimpleMove}
import it.scalachess.core.pieces._
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelTwo {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def willNotBeTrickedOnTheNextEnemyMove(whiteAI: AI): Unit = {
    it should "not be tricked very easily: it should not lose a queen to capture a pawn" in {
      info("The white player has only two pieces: the king and a queen")
      info("The black player has only three pieces: the king and two pawn")
      info("The kings are isolated: the white is in right down corner; the black is in the right up corner")
      info("One of the black pawn protect the other, and this last one is exposed to the white queen attack")
      info("To avoid this naive trap and have this test passed, it's necessary a minimax depth = 2")
      Given("the specific board previously described")
      val blackPawnExposedPosition = Position(5, 6)
      val whiteQueen = Piece(White, Queen)
      val blackPawn = Piece(Black, Pawn)
      val pieceMap = Map(Position(8, 1) -> Piece(White, King), Position(8, 8) -> Piece(Black, King),
        Position(3, 4) -> whiteQueen, blackPawnExposedPosition -> blackPawn, Position(6, 7) -> blackPawn)
      var board = Board(pieceMap)
      val history = Seq( // the history needs to contains a move for each king, to prevent the castling move generation
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      )

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board will still contains the black pawn which was exposed before")
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(blackPawn))
    }
  }

  def generateLastFoolsMateMove(blackAI: AI) {
    it should "generate the Fool's mate" in {
      info("To generate a checkmate, it's necessary a minimax depth = 2")
      Given("a board with almost the Fool's mate built, except the last move")
      val firstWhitePawnMove = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
      val blackPawnMove = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
      val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
      val blackQueenMove = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      board = board(firstWhitePawnMove.boardChanges)
      board = board(blackPawnMove.boardChanges)
      board = board(secondWhitePawnMove.boardChanges)

      When("the black A.I. generates the move")
      val blackAIMove = blackAI.generateSmartMove(board, history)

      Then("the move should be last Fool's mate move, and cause checkmate")
      blackAIMove.validMove should be(blackQueenMove)
      blackAIMove.resultsInCheckmate should be(true)
    }
  }

  def generateLastScholarsMateMove(whiteAI: AI) {
    it should "generate the Scholar's mate" in {
      info("To generate a checkmate, it's necessary a minimax depth = 2")
      Given("a board with almost the Scholar's mate built, except the last move")
      val firstMoveWhitePawn = ValidSimpleMove(Position(5, 2), Position(5, 4), Pawn, White, None)
      val secondMoveBlackPawn = ValidSimpleMove(Position(5, 7), Position(5, 5), Pawn, Black, None)
      val thirdMoveWhiteBishop = ValidSimpleMove(Position(6, 1), Position(3, 4), Bishop, White, None)
      val fourthMoveBlackKnight = ValidSimpleMove(Position(2, 8), Position(3, 6), Knight, Black, None)
      val fifthMoveWhiteQueen = ValidSimpleMove(Position(4, 1), Position(8, 5), Queen, White, None)
      val sixthMoveBlackKnight = ValidSimpleMove(Position(7, 8), Position(6, 6), Knight, Black, None)
      val seventhMoveWhiteQueen = ValidSimpleMove(Position(8, 5), Position(6, 7), Queen, White, Some(Position(6, 7)))
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      board = board(firstMoveWhitePawn.boardChanges)
      board = board(secondMoveBlackPawn.boardChanges)
      board = board(thirdMoveWhiteBishop.boardChanges)
      board = board(fourthMoveBlackKnight.boardChanges)
      board = board(fifthMoveWhiteQueen.boardChanges)
      board = board(sixthMoveBlackKnight.boardChanges)

      When("the white A.I. generates the move")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      Then("the move should be last Scholar's mate move, and cause checkmate")
      whiteAIMove.validMove should be(seventhMoveWhiteQueen)
      whiteAIMove.resultsInCheckmate should be(true)
    }
  }

  /*
  def willBeTrickedOnTheSecondNextEnemyMove(whiteAI: AI, blackAI: AI) {
    it should "be tricked easily because the mini-max can't see anything after his depth (= 3)" in {
      info("The white player has only three pieces: the king, the queen and a pawn")
      info("The black player has only four pieces: the king and three pawn")
      info("The kings are isolated: the white is in right down corner; the black is in the right up corner")
      info("Two of the black pawn protect the other, and this last one is exposed to the white pawn attack")
      info("Th")
      info("To avoid this trap, it's necessary a minimax depth = 4, but level two has only depth = 3")
      Given("the specific board previously described")
      val blackPawnExposedPos = Position(5, 5)
      val blackPawn = Piece(Black, Pawn)
      val whiteKnight = Piece(White, Knight)
      val pieceMap = Map(Position(1, 8) -> Piece(White, King), Position(8, 8) -> Piece(Black, King),
        Position(4, 3) -> whiteKnight, Position(4, 5) -> whiteKnight,
        blackPawnExposedPos -> blackPawn, Position(6, 6) -> blackPawn, Position(4, 6) -> blackPawn)
      var board = Board(pieceMap)
      val history = Seq( // the history needs to contains a move for each king, to prevent the castling move generation
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      )

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      println(whiteAIMove)
      Then("the board will contains the white rook instead of the black knight (which has been captured)")
      board.pieceAtPosition(blackPawnExposedPos) should be(Some(whiteKnight))

      And("after that, on black turn, the white rook will be captured by the other black rook! \n" +
        "So the white A.I. loses a rook (value = 50) to capture a knight (value = 30)")
      board.pieces.values.exists(_ == whiteKnight) should be (true)
      val blackMove = blackAI.generateSmartMove(board, history)
      println(blackMove)
      board = board(blackMove.validMove.boardChanges)
      board.pieceAtPosition(blackPawnExposedPos) should be(Some(blackPawn))
      board.pieces.values.exists(_ == whiteKnight) should be (false)
    }
  }
  */

}