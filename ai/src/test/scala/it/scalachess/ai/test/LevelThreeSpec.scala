package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.ai.test.specifics.{LevelOne, LevelThree, LevelTwo, LevelZero, WrongUsage}
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

final case class LevelThreeSpec() extends FlatSpec with Matchers with GivenWhenThen
  with WrongUsage with LevelZero with LevelOne with LevelTwo with LevelThree {

  private val level = 3
  private val whiteAI = AI(level, White)
  private val blackAI = AI(level, Black)

  // lv 0 tests
  "The level two chess A.I." should behave like generateMove(whiteAI, blackAI)

  // lv 1 tests
  it should behave like generateSimpleCapture(whiteAI)
  it should behave like generateTheMostValuedCapture(whiteAI)
  // this test will fail due to the minimax depth = 2
  // it should behave like willBeTrickedOnTheVeryNextOpponentMove(whiteAI, blackAI)

  // lv 2 tests
  // this test will success due to the minimax depth = 2
  it should behave like willNotBeTrickedOnTheNextEnemyMove(whiteAI)

  // lv 3 tests
  // this tests will success due to the minimax depth = 2
  it should behave like generateLastFoolsMateMove(blackAI)
  it should behave like generateLastScholarsMateMove(whiteAI)

  // lv 4 tests
  // since the A.I. generates a random move, it could pass this test
  // it should behave like generateKnightMoveAtTheStart(whiteAI)

  // wrong usages tests
  it should behave like chessAICantBeUsedDuringCheckmate(whiteAI, blackAI)

}