package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.core.{Black, White}
import it.scalachess.ai.test.specifics.{LevelZero, WrongUsage}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

final case class LevelZeroSpec() extends FlatSpec with Matchers with GivenWhenThen
  with WrongUsage with LevelZero {

  private val level = 0
  private val whiteAI = AI(level, White)
  private val blackAI = AI(level, Black)

  // lv 0 tests
  "The level zero chess A.I." should behave like generateMove(whiteAI, blackAI)

  // since the A.I. generates a random move, it could pass all the other tests

  // lv 1 tests
  // it should behave like generateSimpleCapture(whiteAI)
  // it should behave like generateTheMostValuedCapture(whiteAI)
  // it should behave like willBeTrickedOnTheVeryNextOpponentMove(whiteAI, blackAI)

  // lv 2 tests
  // this test will never succeed because this A.I. isn't smart enough
  // it should behave like willNotBeTrickedOnTheNextEnemyMove(whiteAI)

  // lv 3 tests
  // since the A.I. generates a random move, it could pass the checkmate tests
  // it should behave like generateLastFoolsMateMove(blackAI)
  // it should behave like generateLastScholarsMateMove(whiteAI)

  // lv 4 tests
  // it should behave like generateKnightMoveAtTheStart(whiteAI)

  // wrong usages tests
  it should behave like chessAICantBeUsedDuringCheckmate(whiteAI, blackAI)

}