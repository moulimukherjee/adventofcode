package y2020.d10

import common.Runner

import scala.collection.mutable

object Main extends Runner {

  override def first(lines: List[String]): Long = {
    val list = List(0L) ++ lines.map(_.toLong).sorted
    val one = list.sliding(2).count { l =>
      l.tail.head - l.head == 1
    }
    val three = list.sliding(2).count { l =>
      l.tail.head - l.head == 3
    }
    one * (three + 1)
  }

  override def second(lines: List[String]): Long = {
    val nums = List(0L) ++ lines.map(_.toLong).sorted ++ List(
      lines.map(_.toLong).max + 3
    )

    val cache = mutable.Map.empty[Int, Long]

    def pathsToLeaf(i: Int): Long = {
      if (i == 0) 1L
      else {
        if (cache.get(i).isEmpty) {
          val curr = nums(i)

          val branches = 1.to(3).map { j =>
            if (i - j >= 0 && curr - nums(i - j) <= 3)
              pathsToLeaf(i - j)
            else 0L
          }

          cache.put(i, branches.sum)
        }

        cache.get(i).get
      }
    }

    pathsToLeaf(nums.size - 1)
  }

  override def testInput: String =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

}
