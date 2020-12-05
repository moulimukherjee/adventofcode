package y2020.d05

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines().toList

    // first
    val max = inputs.map(seatId).max
    println(s"max ${max}")

    // second
    val neighbours =
      inputs.map(seatId).sorted.sliding(2).find(l => l.tail.head - l.head == 2)
    println(s"seat = ${neighbours.get.head + 1}")
  }

  def seatId(str: String): Long = {
    val binary = str
      .replaceAll("F", "0")
      .replaceAll("B", "1")
      .replaceAll("R", "1")
      .replaceAll("L", "0")

    val column = Integer.parseInt(binary.substring(0, 7), 2)
    val row = Integer.parseInt(binary.substring(7), 2)

    (column * 8) + row
  }
}
