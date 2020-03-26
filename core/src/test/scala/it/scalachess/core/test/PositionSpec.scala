package it.scalachess.core.test

import it.scalachess.core.board.{ Board, Position }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class PositionSpec extends FlatSpec with Matchers with OptionValues with Inspectors {
  val columnLetters: Map[String, Int] =
    Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5, "f" -> 6, "g" -> 7, "h" -> 8)

  val allPositions: Seq[Position] = {
    for (row <- 1 to Board.width; col <- 1 to Board.height) yield Position.of(row)(col)
  }.flatten

  "Each position expressed as a string" should "have the correct coordinate values" in {
    forAll(columnLetters.toSet) {
      case (col: String, colValue: Int) =>
        forAll(1 to Board.height) { row =>
          val pos = Position.ofNotation(s"$col$row")
          pos should be('defined)
          pos.value.row should equal(row)
          pos.value.col should equal(colValue)
        }
    }
  }

  "up, down, left, right" should "be idempotent" in {
    val c5                = Position.ofNotation("c5").get
    val upDownC5          = c5.up.value.down.value
    val leftRightC5       = c5.left.value.right.value
    val upLeftDownRightC5 = c5.upLeft.value.downRight.value
    val upRightDownLeftC5 = c5.upRight.value.downLeft.value

    all(Seq(upDownC5, leftRightC5, upLeftDownRightC5, upRightDownLeftC5)) should equal(c5)
  }

  "Positions outside the board" should "not be defined" in {
    val outerDownRow = allPositions
      .filter { _.row == 1 }
      .flatMap { p =>
        Seq(p.downLeft, p.downRight, p.down)
      }

    val outerUpRow = allPositions
      .filter { _.row == Board.height }
      .flatMap { p =>
        Seq(p.upLeft, p.upRight, p.up)
      }

    val outerLeftColumn = allPositions
      .filter { _.col == 1 }
      .flatMap { p =>
        Seq(p.downLeft, p.upLeft, p.left)
      }

    val outerRightColumn = allPositions
      .filter { _.col == Board.width }
      .flatMap { p =>
        Seq(p.downRight, p.upRight, p.right)
      }

    val outerPositions = outerDownRow ++ outerUpRow ++ outerLeftColumn ++ outerRightColumn
    outerPositions should not be empty
    all(outerPositions) should not be 'defined
  }

  "rowDistance between positions on different rows" should "be equal to the difference in row value" in {
    val rowValueCombinations = (1 to Board.height).combinations(2).toList
    forAll(rowValueCombinations) {
      case Vector(rowN1: Int, rowN2: Int) =>
        val row_1 = (1 to Board.width) flatMap { Position.of(_)(rowN1) }
        row_1 should not be empty
        forAll(row_1) { p =>
          p.row should equal(rowN1)
        }

        val row_2 = (1 to Board.width) flatMap { Position.of(_)(rowN2) }
        row_2 should not be empty
        forAll(row_2) { p =>
          p.row should equal(rowN2)
        }

        row_1.size should equal(row_2.size)
        forAll(row_1 zip row_2) {
          case (pos1: Position, pos2: Position) =>
            pos1.rowDistanceAbs(pos2) should be(math.abs(rowN1 - rowN2))
        }
    }
  }

  "colDistance between positions on different columns" should "be equal to the difference in column value" in {
    val columnValueCombinations = (1 to Board.width).combinations(2).toList
    forAll(columnValueCombinations) {
      case Vector(colN1: Int, colN2: Int) =>
        val col_1 = (1 to Board.height) flatMap { Position.of(colN1) }
        col_1 should not be empty
        forAll(col_1) { p =>
          p.col should equal(colN1)
        }

        val col_2 = (1 to Board.height) flatMap { Position.of(colN2) }
        col_2 should not be empty
        forAll(col_2) { p =>
          p.col should equal(colN2)
        }

        col_1.size should equal(col_2.size)

        forAll(col_1 zip col_2) {
          case (pos1: Position, pos2: Position) =>
            pos1.colDistanceAbs(pos2) should be(math.abs(colN1 - colN2))
        }
    }
  }

  "Each position on the board" should "be straight to all the positions on the relative row and column" in {
    forAll(allPositions) { pos: Position =>
      val straightPositions = allPositions.filter { p =>
        p.col == pos.col || p.row == pos.row
      }
      forAll(straightPositions) { pos2 =>
        assert(pos.isStraightTo(pos2))
      }
    }
  }

  it should "be diagonal to all the positions on the relative diagonals" in {
    forAll(allPositions) { pos: Position =>
      val straightPositions = allPositions.filter { p =>
        pos.rowDistanceAbs(p) === pos.colDistanceAbs(p)
      }
      forAll(straightPositions) { pos2 =>
        assert(pos.isDiagonalTo(pos2))
      }
    }
  }
}
