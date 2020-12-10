package y2020.d05

import common.Runner

object Main extends Runner {

  override def first(lines: List[String]): Long = lines.map(seatId).max

  override def second(lines: List[String]): Long = {
    val neighbours =
      lines.map(seatId).sorted.sliding(2).find(l => l.tail.head - l.head == 2)
    neighbours.get.head + 1
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
