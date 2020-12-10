package y2020.d06

import common.Runner

object Main extends Runner {

  override def first(lines: List[String]): Long =
    passwordGroups(lines).map { list =>
      list.mkString("").toCharArray.toSet.size
    }.sum

  override def second(lines: List[String]): Long =
    passwordGroups(lines).map { list =>
      list.map(_.toSet).reduce((a, b) => a.intersect(b)).size
    }.sum

  def passwordGroups(lines: List[String]) =
    lines.mkString("\n").split("\n\n").map(_.split("\n"))

}
