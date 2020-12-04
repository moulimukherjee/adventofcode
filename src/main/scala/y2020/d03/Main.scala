package y2020.d03

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines()

    val pattern = inputs.map(_.toCharArray).toArray
    val maxRows = pattern.size
    val maxColumns = pattern.head.size

    // first
    val treeCount = 0.until(maxRows).count { row =>
      val column = (3 * row) % maxColumns
      pattern(row)(column) == '#'
    }
    println(s"first result ${treeCount}")

    // second
    def countTrees(stepRight: Int, stepDown: Int): Long = {
      0.until(maxRows)
        .by(stepDown)
        .zipWithIndex
        .count {
          case (r, i) =>
            val c = (stepRight * i) % maxColumns
            pattern(r)(c) == '#'
        }
        .toLong
    }

    val secondResult = countTrees(1, 1) *
      countTrees(3, 1) *
      countTrees(5, 1) *
      countTrees(7, 1) *
      countTrees(1, 2)
    println(s"second result ${secondResult}")

  }
}
