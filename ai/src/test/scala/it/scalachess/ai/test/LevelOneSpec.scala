package it.scalachess.ai.test

import it.scalachess.ai.AI
import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import it.scalachess.core.{Black, White}
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.{FullMove, ValidSimpleMove}
import it.scalachess.core.pieces.{King, Pawn, Piece, Queen}

class LevelOneSpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("the level one AI plays the move which capture the more important piece") {

    scenario("it generates the move which capture a piece, in a standard game simulation") {

      Given("a board with the white player having the possibility to capture a piece")
      var board = Board.defaultBoard()
      val history = Seq() // the history doesn't need to be updated in this test
      val firstWhitePawnMove = ValidSimpleMove(Position(2, 2), Position(2, 4), Pawn, White, None)
      val blackPawnMove = ValidSimpleMove(Position(3, 7), Position(3, 5), Pawn, Black, None)
      board = board(firstWhitePawnMove.boardChanges)
      board = board(blackPawnMove.boardChanges)

      Given("the white A.I. representing the player which can capture")
      val whiteAI = AI(1, White)

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board should contains the white piece instead of the black one (which has been captured)")
      board.pieceAtPosition(blackPawnMove.to) should be(Some(Piece(firstWhitePawnMove.color, firstWhitePawnMove.pieceType)))
    }

    scenario("this A.I. can be tricked very easily: it lose a queen to capture a pawn!") {

      info("The white player has only two pieces: the king and a queen")
      info("The black player has only three pieces: the king and two pawn")
      info("The kings are isolated: white is in right down corner; black is in the right up corner")
      info("One of the black pawn protect the other, and this last one is exposed to the white queen attack")

      Given("the specific board previously described")
      val blackPawnExposedPosition = Position(5, 6)
      val whiteQueen = Piece(White, Queen)
      val blackPawn = Piece(Black, Pawn)
      val pieceMap = Map(Position(8,1) -> Piece(White, King), Position(8,8) -> Piece(Black, King),
        Position(3, 4) -> whiteQueen, blackPawnExposedPosition -> blackPawn, Position(6,7) -> blackPawn)
      var board = Board(pieceMap)
      val history = Seq(
        FullMove(ValidSimpleMove(Position(1,1), Position(1,1), King, White, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map())),
        FullMove(ValidSimpleMove(Position(1,1), Position(1,1), King, Black, None), resultsInCheck = false, resultsInCheckmate = false, Board(Map()))
      ) // the history needs to contains a move for each king, to prevent the castling move generation

      Given("the white A.I. representing the player")
      val whiteAI = AI(1, White)

      When("the white A.I. generates the move, and it's applied to the board")
      val whiteAIMove = whiteAI.generateSmartMove(board, history)
      board = board(whiteAIMove.validMove.boardChanges)

      Then("the board will contains the white queen instead of the black pawn exposed (which has been captured)")
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(whiteQueen))

      And("after that, on black turn, the white queen will be captured by the other black pawn")
      board.pieces.values.exists(_ == whiteQueen) should be (true)
      val blackAI = AI(1, Black)
      val blackMove = blackAI.generateSmartMove(board, history)
      board = board(blackMove.validMove.boardChanges)
      board.pieceAtPosition(blackPawnExposedPosition) should be(Some(blackPawn))
      board.pieces.values.exists(_ == whiteQueen) should be (false)
    }
  }

}
