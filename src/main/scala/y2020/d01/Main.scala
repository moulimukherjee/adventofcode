package y2020.d01

import common.Runner

object Main extends Runner {

  override def first(lines: List[String]): Long = {
    val nums = lines.map(_.toLong).toSet
    nums.find(num => nums(2020 - num)).map(n => (2020 - n) * n).get
  }

  override def second(lines: List[String]): Long = {
    val nums = lines.map(_.toLong).toSet

    nums
      .flatMap { n =>
        val o = nums.find(m => nums(2020 - n - m))
        if (o.nonEmpty) Some(n, o.get) else None
      }
      .toList
      .headOption
      .map {
        case (m, n) => (2020 - n - m) * n * m
      }
      .get
  }

  override val testInput =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin
}
