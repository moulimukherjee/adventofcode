package y2020.d09

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines().toArray

    val nums = inputs.map(_.toLong)

    val invalidNum = nums.zipWithIndex
      .takeRight(nums.size - 25)
      .find {
        case (n, i) =>
          val (start, end) = (i - 25, i)
          !valid(nums.slice(start, end + 1).toSet, n)
      }
      .map(_._1)
      .get

    println(s"first ${invalidNum}")

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

    val weakness = slice.min + slice.max

    println(s"second ${weakness}")

  }

  def valid(nums: Set[Long], sum: Long): Boolean = {
    nums.find(n => nums.contains(sum - n)).nonEmpty
  }

}
