package y2020.d03

import common.Runner

object Main extends Runner {

  override def first(lines: List[String]): Long = {
    val pattern = lines.map(_.toCharArray).toArray
    0.until(pattern.size).count { row =>
      val column = (3 * row) % pattern.head.size
      pattern(row)(column) == '#'
    }
  }

  override def second(lines: List[String]): Long = {
    val pattern = lines.map(_.toCharArray).toArray

    def countTrees(stepRight: Int, stepDown: Int): Long = {
      0.until(pattern.size)
        .by(stepDown)
        .zipWithIndex
        .count {
          case (r, i) =>
            val c = (stepRight * i) % pattern.head.size
            pattern(r)(c) == '#'
        }
        .toLong
    }
    countTrees(1, 1) *
      countTrees(3, 1) *
      countTrees(5, 1) *
      countTrees(7, 1) *
      countTrees(1, 2)
  }

  override val testInput: String = """..##.......
                                      |#...#...#..
                                      |.#....#..#.
                                      |..#.#...#.#
                                      |.#...##..#.
                                      |..#.##.....
                                      |.#.#.#....#
                                      |.#........#
                                      |#.##...#...
                                      |#...##....#
                                      |.#..#...#.#""".stripMargin

}
