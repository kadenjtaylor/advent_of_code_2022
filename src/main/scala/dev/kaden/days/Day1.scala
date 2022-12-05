package dev.kaden.days

import cats.effect.IO

object Day1 {

  object Part1 {

    object Domain {
      case class Elf(snacks: List[Snack]) {
        def totalCalories: Int = snacks.map(_.calories).sum
      }

      case class Snack(calories: Int)

      private def isEmpty: String => Boolean = _ == ""

      def parseToElfSnacks(lines: List[String]): IO[List[Elf]] = IO {
        val chunks = Util.split(lines)(isEmpty)
        chunks.map(l => Elf(l.map(s => Snack(Integer.parseInt(s)))))
      }
    }

    object Util {
      def readLines(filename: String): IO[List[String]] = {
        import scala.io.Source
        IO {
          Source.fromFile(filename).getLines().toList
        }
      }

      private case class ListAcc[X](done: Option[List[List[X]]], going: List[X]) {
        def value = done.map(_ :+ going).getOrElse(List())
      }
      def split[X](l: List[X])(pred: X => Boolean) = {
        val init = ListAcc[X](None, List())
        l.foldLeft(init)((acc, x) => {
          (pred(x), acc) match {
            case (true, ListAcc(anything, List()))  => ListAcc(anything, List())
            case (true, ListAcc(None, done))        => ListAcc(Some(List(done)), List())
            case (true, ListAcc(Some(done), going)) => ListAcc(Some(done :+ going), List())
            case (false, ListAcc(done, going))      => ListAcc(done, going :+ x)
          }
        }).value
      }

      def sumOfTopN(ns: List[Int], n: Int) = ns.sorted(Ordering.Int.reverse).take(n)
    }

    val n = 3

    def run(): IO[Unit] = for {
      lines <- Util.readLines("src/main/resources/input/day_1.txt")
      elves <- Domain.parseToElfSnacks(lines)
      top   <- IO.pure(Util.sumOfTopN(elves.map(_.totalCalories), n))
      _     <- IO.println(s"Top $n: $top")
      _     <- IO.println(s"Total: ${top.sum}")
    } yield ()
  }

}
