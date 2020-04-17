package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.ai.level.Level
import it.scalachess.core.{Black, White}
import it.scalachess.core.board.{Board, Position}
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{Pawn, Queen}
import org.scalatest.{FlatSpec, Inspectors, Matchers, OptionValues}

class LevelZeroSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "A chess A.I. used during checkmate means a wrong usage. It" should "raises exception" in {
    val firstWhitePawnMove  = ValidSimpleMove(Position(6, 2), Position(6, 3), Pawn, White, None)
    val blackPawnMove       = ValidSimpleMove(Position(5, 7), Position(5, 6), Pawn, Black, None)
    val secondWhitePawnMove = ValidSimpleMove(Position(7, 2), Position(7, 4), Pawn, White, None)
    val blackQueenMove      = ValidSimpleMove(Position(4, 8), Position(8, 4), Queen, Black, None)
    var board               = Board.defaultBoard()
    val history             = Seq()
    val whiteAI             = AI(0, White)
    val blackAI             = AI(0, Black)
    board = board(firstWhitePawnMove.boardChanges)
    board = board(blackPawnMove.boardChanges)
    board = board(secondWhitePawnMove.boardChanges)
    board = board(blackQueenMove.boardChanges)
    var whiteAIResult = "success"
    try {whiteAI.generateSmartMove(board, history)} catch {
      case e: IllegalArgumentException => whiteAIResult = e.getMessage
    }
    whiteAIResult should be ("requirement failed: " + Level.aiIsInCheckmateErrorMsg)
    var blackAIResult = "success"
    try {blackAI.generateSmartMove(board, history)} catch {
      case e: IllegalArgumentException => blackAIResult = e.getMessage
    }
    blackAIResult should be ("requirement failed: " + Level.opponentIsInCheckmateErrorMsg)
  }

  "A level zero chess A.I" should "generate a random move" in {
    val defaultBoard = Board.defaultBoard()
    val player       = White
    val history      = Seq()
    val ai           = AI(0, player)
    val moves = new MoveGenerator(defaultBoard, player, history).allMoves()
    moves.nonEmpty should be(true)
    moves.contains(ai.generateSmartMove(defaultBoard, history)) should be(true)
  }



}
