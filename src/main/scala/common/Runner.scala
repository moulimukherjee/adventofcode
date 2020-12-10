package common

import scala.io.Source
import scala.jdk.CollectionConverters._

trait Runner {
  val inputLines =
    Source.fromURL(getClass.getResource("input.txt")).getLines().toList

  def main(args: Array[String]): Unit = {
    adhocTest

    if (!testInput.isEmpty)
      println(
        s"(Test) First Part: ${first(testInput.lines().iterator().asScala.toList)}"
      )
    println(s"Actual First Part: ${first(inputLines)}")

    println

    if (!testInput.isEmpty)
      println(
        s"(Test) Second Part: ${second(testInput.lines().iterator().asScala.toList)}"
      )
    println(s"Actual Second Part: ${second(inputLines)}")

  }

  def first(lines: List[String]): Long
  def second(lines: List[String]): Long

  def testInput: String = ""

  def adhocTest: Unit = {}

}
