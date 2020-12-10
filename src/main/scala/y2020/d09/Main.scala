package y2020.d09

import common.Runner

object Main extends Runner {

  def findInvalid(nums: List[Long], preamble: Int) = {
    nums.zipWithIndex
      .takeRight(nums.size - preamble)
      .find {
        case (n, i) =>
          val (start, end) = (i - preamble, i)
          !valid(nums.slice(start, end + 1).toSet, n)
      }
      .map(_._1)
      .get

  }

  override def first(lines: List[String]): Long =
    findInvalid(lines.map(_.toLong), 25)

  override def second(lines: List[String]): Long = {
    val invalidNum = first(lines)
    val nums = lines.map(_.toLong)
    val indicesRange = nums.zipWithIndex
      .flatMap {
        case (n, i) =>
          LazyList
            .iterate((i, n)) {
              case (index, sum) => (index + 1, sum + nums(index + 1))
            }
            .takeWhile(
              sumIndex =>
                sumIndex._2 <= invalidNum && sumIndex._1 <= nums.size - 1
            )
            .lastOption match {
            case Some((lastIndex, total)) if (total == invalidNum) =>
              List((i, lastIndex))
            case _ => List.empty
          }
      }
      .filter(p => p._1 != p._2)
      .head

    val slice = nums.slice _ tupled indicesRange

    slice.min + slice.max
  }

  def valid(nums: Set[Long], sum: Long): Boolean =
    nums.find(n => nums.contains(sum - n)).nonEmpty

  override def adhocTest: Unit = {
    val lines = """35
                  |20
                  |15
                  |25
                  |47
                  |40
                  |62
                  |55
                  |65
                  |95
                  |102
                  |117
                  |150
                  |182
                  |127
                  |219
                  |299
                  |277
                  |309
                  |576""".stripMargin
      .split("\n")
      .toList
      .map(_.toLong)

    assert(findInvalid(lines, 5) == 127)
  }

}
