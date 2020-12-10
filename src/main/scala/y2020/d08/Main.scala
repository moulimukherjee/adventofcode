package y2020.d08

import common.Runner

import scala.collection.mutable

object Main extends Runner {
  override def first(lines: List[String]): Long = calc(lines.toArray)._1

  override def second(lines: List[String]): Long = {
    lines.zipWithIndex
      .filter(s => s._1.startsWith("jmp") || s._1.startsWith("nop"))
      .map {
        case (op, index) =>
          val replacedArray =
            if (lines(index).startsWith("jmp"))
              lines.updated(index, op.replace("jmp", "nop"))
            else lines.updated(index, op.replace("nop", "jmp"))

          calc(replacedArray.toArray)
      }
      .find(_._2 == lines.size - 1)
      .map(_._1)
      .get
  }

  def calc(inputs: Array[String]) = {
    val state = mutable.Set.empty[Int]
    LazyList
      .iterate((0, 0)) {
        case (acc, i) =>
          state.add(i)
          val parseOp = inputs(i).split("\\s", 2)
          parseOp.head match {
            case "nop" => (acc, i + 1)
            case "acc" => (acc + parseOp.tail.head.toInt, i + 1)
            case "jmp" => (acc, i + parseOp.tail.head.toInt)
          }
      }
      .takeWhile(l => !state.contains(l._2) && l._2 < inputs.size)
      .last
  }

  override val testInput: String = """nop +0
                                      |acc +1
                                      |jmp +4
                                      |acc +3
                                      |jmp -3
                                      |acc -99
                                      |acc +1
                                      |jmp -4
                                      |acc +6""".stripMargin
}
