package y2020.d02

import common.Runner

object Main extends Runner {
  val format = "(\\d+)-(\\d+)\\s(\\w):\\s(\\w+)".r

  override def first(lines: List[String]): Long = {
    lines.count { str =>
      str match {
        case format(min, max, letter, pwd) =>
          val count = pwd.toCharArray.count(_ == letter.toCharArray.head)
          (min.toInt <= count) && (count <= max.toInt)
        case _ => sys.error("could not match")
      }
    }
  }

  override def second(lines: List[String]): Long = {
    lines.count { str =>
      str match {
        case format(min, max, letter, pwd) =>
          val first = pwd.length >= min.toInt && pwd.charAt(min.toInt - 1) == letter.toCharArray.head
          val second = pwd.length >= max.toInt && pwd.charAt(max.toInt - 1) == letter.toCharArray.head
          first ^ second
        case _ => sys.error("could not match")
      }
    }
  }

  override val testInput =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin

}
