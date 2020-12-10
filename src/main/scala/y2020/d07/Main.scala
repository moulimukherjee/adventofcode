package y2020.d07

import common.Runner

object Main extends Runner {

  override def first(lines: List[String]): Long = {
    val reverseBagMap =
      lines
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

    bags.size
  }

  override def second(lines: List[String]): Long = {
    val bagCountMap = lines
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
  }

}
