package it.scalachess.ai.test.specifics

import it.scalachess.ai.AI
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.{FullMove, ValidSimpleMove}
import it.scalachess.core.pieces.{Bishop, King, Pawn, Piece, Queen, Rook}
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

trait LevelOne {
  this: FlatSpec with Matchers with GivenWhenThen =>

  def generateSimpleCapture(whiteAI: AI) {
    it should "generates the move which capture a piece, in a standard game simulation" in {
      Given("a default board having the white player with the possibility to capture a piece")
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      val firstWhitePawnMove = ValidSimpleMove(Position(2, 2), Position(2, 4), Pawn, White, None)
      val blackPawnMove = ValidSimpleMove(Position(3, 7), Position(3, 5), Pawn, Black, None)
      board = board(firstWhitePawnMove.boardChanges)
      board = board(blackPawnMove.boardChanges)

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board should contains the white piece instead of the black one (which has been captured)")
      board.pieceAtPosition(blackPawnMove.to) should be(Some(Piece(firstWhitePawnMove.color, firstWhitePawnMove.pieceType)))
    }
  }

  def generateTheMostValuedCapture(whiteAI: AI) {
    it should "generates the move which captures the most valued piece" in {
      info("The white player has only two pieces: the king and a queen")
      info("The black player has a rook and a bishop, plus his king")
      info("The kings are isolated: the white is in right down corner; the black is in the right up corner")
      info("The black rook and the black bishop are exposed to the white queen attack")
      info("The A.I. white player will move")
      info("The rook has a major value respect the bishop")
      Given("the specific board previously described")
      val whiteQueen = Piece(White, Queen)
      val blackRook = Piece(Black, Rook)
      val blackRookPos = Position(3, 4)
      val pieceMap = Map(Position(8, 1) -> Piece(White, King), Position(8, 8) -> Piece(Black, King),
        Position(3, 3) -> whiteQueen, blackRookPos -> blackRook, Position(4, 4) -> Piece(Black, Bishop))
      var board = Board(pieceMap)
      val history = Seq( // the history needs to contains a move for each king, to prevent the castling move generation
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1, 1), Position(1, 1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      )

      When("the white A.I. generates the move, and it's applied to the board ")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      println(whiteAIMove)
      Then("the board will contains the white queen instead of the black rook (which has been captured)")
      board.pieceAtPosition(blackRookPos) should be(Some(whiteQueen))
    }
  }

  def willBeTrickedOnTheVeryNextOpponentMove(whiteAI: AI, blackAI: AI) {
    it should "cares only to capture a piece, so: it should lose a queen to capture a pawn" in {
      info("The white player has only two pieces: the king and a queen")
      info("The black player has only three pieces: the king and two pawn")
      info("The kings are isolated: the white is in right down corner; the black is in the right up corner")
      info("One of the black pawn protect the other, and this last one is exposed to the white queen attack")
      info("The A.I. white player will move")
      Given("the specific board previously described")
      val blackPawnExposedPosition = Position(5, 6)
      val whiteQueen = Piece(White, Queen)
      val blackPawn = Piece(Black, Pawn)
      val pieceMap = Map(Position(8,1) -> Piece(White, King), Position(8,8) -> Piece(Black, King),
        Position(3, 4) -> whiteQueen, blackPawnExposedPosition -> blackPawn, Position(6,7) -> blackPawn)
      var board = Board(pieceMap)
      val history = Seq( // the history needs to contains a move for each king, to prevent the castling move generation
        FullMove(ValidSimpleMove(Position(1,1), Position(1,1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1,1), Position(1,1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      )

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board will contains the white queen instead of the black pawn exposed (which has been captured)")
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(whiteQueen))

      And("after that, on black turn, the white queen will be captured by the other black pawn")
      board.pieces.values.exists(_ == whiteQueen) should be (true)
      val blackMove = blackAI.generateSmartMove(board, history)
      board = board(blackMove.validMove.boardChanges)
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(blackPawn))
      board.pieces.values.exists(_ == whiteQueen) should be (false)
    }
  }

}
