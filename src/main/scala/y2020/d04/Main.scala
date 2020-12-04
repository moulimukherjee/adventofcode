package y2020.d04

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines()

    val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    val optionalKeys = Set("cid")

    val passportStrings = ListBuffer[String]()
    val currentString = new StringBuilder()
    while (inputs.hasNext) {
      val str = inputs.next()
      if (str == "") {
        passportStrings.append(currentString.deleteCharAt(0).toString())
        currentString.clear()
      } else {
        currentString.append(" ").append(str)
      }
    }

    val passports = passportStrings.map { str =>
      str
        .split("\\s")
        .map { pair =>
          val outs = pair.split(":", 2)
          assert(outs.size == 2, s"${pair} is invalid in ${str}")
          outs(0) -> outs(1)
        }
        .toMap

    }

    // first
    val firstCount = passports.count(p => requiredKeys.subsetOf(p.keySet))
    println(s"first count ${firstCount}")

    // second
    def isValid(map: Map[String, String]): Boolean = {
      val bools = map.iterator.map {
        case (key, value) if (key == "byr") =>
          Try(value.toInt).map(i => i >= 1920 && i <= 2002).getOrElse(false)
        case (key, value) if (key == "iyr") =>
          Try(value.toInt).map(i => i >= 2010 && i <= 2020).getOrElse(false)
        case (key, value) if (key == "eyr") =>
          Try(value.toInt).map(i => i >= 2020 && i <= 2030).getOrElse(false)
        case (key, value) if (key == "hgt") =>
          val heightRegex = "(\\d+)(\\w+)".r
          value match {
            case heightRegex(height, unit) if unit == "cm" =>
              Try(height.toInt).map(i => i >= 150 && i <= 193).getOrElse(false)
            case heightRegex(height, unit) if unit == "in" =>
              Try(height.toInt).map(i => i >= 59 && i <= 76).getOrElse(false)
            case _ => false
          }
        case (key, value) if (key == "hcl") => value.matches("#[0-9a-f]{6}")
        case (key, value) if (key == "ecl") =>
          value.matches("amb|blu|brn|gry|grn|hzl|oth")
        case (key, value) if (key == "pid") => value.matches("\\d{9}")
        case _                              => true
      }
      bools.reduce(_ && _)
    }

    val secondCount = passports.count { p =>
      requiredKeys.subsetOf(p.keySet) && isValid(p)
    }

    println(s"second result ${secondCount}")

  }
}
