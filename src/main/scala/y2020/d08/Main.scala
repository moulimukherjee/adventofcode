package y2020.d08

import scala.collection.mutable
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val inputs =
      Source.fromURL(getClass.getResource("input.txt")).getLines().toArray

    // first
    val first = calc(inputs)._1
    println(s"first ${first}")

    // second
    val second = inputs.zipWithIndex
      .filter(s => s._1.startsWith("jmp") || s._1.startsWith("nop"))
      .map {
        case (op, index) =>
          val replacedArray =
            if (inputs(index).startsWith("jmp"))
              inputs.updated(index, op.replace("jmp", "nop"))
            else inputs.updated(index, op.replace("nop", "jmp"))

          calc(replacedArray)
      }
      .find(_._2 == inputs.size - 1)
      .map(_._1)

    println(s"second ${second}")
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
}
