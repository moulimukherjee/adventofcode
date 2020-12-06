package y2020.d06

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines()

    def splitByBlanks(lines: Iterator[String]): List[List[String]] = {
      val entries = ListBuffer(ListBuffer[String]())
      lines.foreach { s =>
        if (s.isEmpty) {
          if (entries.last.nonEmpty) entries.append(ListBuffer[String]())
        } else {
          entries.last.append(s)
        }
      }
      entries.map(_.toList).toList
    }

    val entries = splitByBlanks(inputs)

    // first
    val output = entries.map { list =>
      list.mkString("").toCharArray.toSet.size
    }.sum

    println(s"output ${output}")

    // second
    val secondSum = entries.map { list =>
      list.map(_.toSet).reduce((a, b) => a.intersect(b)).size
    }.sum

    println(s"second sum ${secondSum}")
  }
}
