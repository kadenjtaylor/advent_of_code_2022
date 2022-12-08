package dev.kaden

import cats.effect.IO
import weaver._
import dev.kaden.days.Day1.Part1.Util

object HelloWorldSuite extends SimpleIOSuite {

  // Easy to test stuff with no side effects...
  pureTest("Ensure 1 + 1 = 2") {
    expect(1 + 1 == 2)
  }

  val baseString = "a,b,,c,,,d"
  val stringSeq  = baseString.map(_.toString())

  pureTest("String Split") {
    val splitted = baseString.split(",").toSeq
    splitted.foreach(s => println(s"'$s'"))
    expect(splitted == Seq("a", "b", "", "c", "", "", "d"))
  }

  pureTest("Sequence Split") {
    val splitted = Util.split(stringSeq)(_ == ",")
    expect(splitted == Seq(Seq("a"), Seq("b"), Seq(""), Seq("c"), Seq(""), Seq(""), Seq("d")))
  }

}
