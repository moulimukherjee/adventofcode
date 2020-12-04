package y2020.d01

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs = Source.fromURL(getClass.getResource("input.txt")).getLines()
    val nums = inputs.map(_.toLong).toSet

    // first
    nums.find(num => nums(2020 - num)) match {
      case Some(n) => println(s"$n x ${2020 - n} = ${(2020 - n) * n}")
      case None    => sys.error("failed!")
    }

    // second
    val output = nums.flatMap { n =>
      val o = nums.find(m => nums(2020 - n - m))
      if (o.nonEmpty) Some(n, o.get) else None
    }

    output.toList.headOption match {
      case Some((m, n)) =>
        println(s"$n x $m x ${2020 - n - m} = ${(2020 - n - m) * n * m}")
      case None => sys.error("failed!")
    }

  }
}
