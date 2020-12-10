package y2020.d04

import common.Runner

import scala.util.Try

object Main extends Runner {
  val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  override def first(lines: List[String]): Long =
    passports(lines).count(p => requiredKeys.subsetOf(p.keySet))

  override def second(lines: List[String]): Long = passports(lines).count { p =>
    requiredKeys.subsetOf(p.keySet) && isValid(p)
  }

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

  def passports(lines: List[String]): List[Map[String, String]] = {
    lines
      .mkString("\n")
      .split("\n\n")
      .map { passportStr =>
        passportStr
          .replaceAll("\n", " ")
          .split("\\s")
          .map { pair =>
            val outs = pair.split(":", 2)
            outs(0) -> outs(1)
          }
          .toMap
      }
      .toList
  }

  override val testInput = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                              |byr:1937 iyr:2017 cid:147 hgt:183cm
                              |
                              |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                              |hcl:#cfa07d byr:1929
                              |
                              |hcl:#ae17e1 iyr:2013
                              |eyr:2024
                              |ecl:brn pid:760753108 byr:1931
                              |hgt:179cm
                              |
                              |hcl:#cfa07d eyr:2025 pid:166559648
                              |iyr:2011 ecl:brn hgt:59in""".stripMargin
}
