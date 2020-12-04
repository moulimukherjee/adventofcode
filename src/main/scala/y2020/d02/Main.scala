package y2020.d02

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines().toList

    val format = "(\\d+)-(\\d+)\\s(\\w):\\s(\\w+)".r

    // first
    val firstCount = inputs.count { str =>
      str match {
        case format(min, max, letter, pwd) =>
          val count = pwd.toCharArray.count(_ == letter.toCharArray.head)
          (min.toInt <= count) && (count <= max.toInt)
        case _ => sys.error("could not match")
      }
    }
    println(s"first count = ${firstCount}")

    // second
    val secondCount = inputs.count { str =>
      str match {
        case format(min, max, letter, pwd) =>
          val first = pwd.length >= min.toInt && pwd.charAt(min.toInt - 1) == letter.toCharArray.head
          val second = pwd.length >= max.toInt && pwd.charAt(max.toInt - 1) == letter.toCharArray.head
          first ^ second
        case _ => sys.error("could not match")
      }
    }
    println(s"second count = ${secondCount}")

  }
}
