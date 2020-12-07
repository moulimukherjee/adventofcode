package y2020.d07

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines().toList

    // first
    val reverseBagMap =
      inputs
        .flatMap { line =>
          val splits = line.split(" bags contain ", 2)
          val regex = "\\d+\\s(\\w+\\s\\w+) bag".r
          regex
            .findAllMatchIn(splits.tail.head)
            .map(m => (m.group(1), splits.head))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap

    val bags = LazyList
      .iterate(reverseBagMap("shiny gold").toSet) { l =>
        l.flatMap { s =>
          reverseBagMap.get(s).toList.flatten.toSet
        }
      }
      .takeWhile(_.nonEmpty)
      .reduce(_.union(_))
      .toList

    println(s"first count ${bags.size}")

    // second
    val bagCountMap = inputs
      .flatMap { line =>
        val splits = line.split(" bags contain ", 2)
        val regex = "(\\d+)\\s(\\w+\\s\\w+) bag".r
        regex
          .findAllMatchIn(splits.tail.head)
          .map(m => splits.head -> (m.group(2) -> m.group(1).toInt))
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    val bagCount =
      LazyList
        .iterate(List(Tuple2(bagCountMap("shiny gold"), 1))) { rows =>
          rows.flatMap {
            case (bagCountPair, multiplier) =>
              bagCountPair.flatMap {
                case (name, count) => {
                  bagCountMap.get(name).toList.map((_, count * multiplier))
                }
              }
          }
        }
        .takeWhile(_.nonEmpty)
        .toList
        .flatMap { r =>
          r.flatMap {
            case (bagCountPair, multiplier) =>
              bagCountPair.map(_._2 * multiplier)
          }
        }
        .sum

    println(s"total count: ${bagCount}")
  }
}
