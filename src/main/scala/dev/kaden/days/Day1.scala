package dev.kaden.days

import cats.effect.IO
import dev.kaden.util.Util.Demo
import dev.kaden.util.Util.readLines

object Day1 {

  object Part1 extends Demo {

    object Domain {
      case class Elf(snacks: Seq[Snack]) {
        def totalCalories: Int = snacks.map(_.calories).sum
      }

      case class Snack(calories: Int)

      def parseToElfSnacks(lines: Seq[String]): IO[Seq[Elf]] = IO {
        val chunks = Util.split(lines)(Util.isEmpty)
        chunks.map(l => Elf(l.map(s => Snack(Integer.parseInt(s)))))
      }
    }

    object Util {

      def isEmpty: String => Boolean = _ == ""

      private case class SeqAcc[X](done: Option[Seq[Seq[X]]], going: Seq[X]) {
        def value = done.map(_ :+ going).getOrElse(List())
      }
      // TODO: Rename this to chunk, implement split, and reimplement chunk as split then filter
      def split[X](l: Seq[X])(pred: X => Boolean) = {
        val init = SeqAcc[X](None, Seq())
        l.foldLeft(init)((acc, x) => {
          (pred(x), acc) match {
            case (true, SeqAcc(anything, Seq()))   => SeqAcc(anything, Seq())
            case (true, SeqAcc(None, done))        => SeqAcc(Some(Seq(done)), Seq())
            case (true, SeqAcc(Some(done), going)) => SeqAcc(Some(done :+ going), Seq())
            case (false, SeqAcc(done, going))      => SeqAcc(done, going :+ x)
          }
        }).value
      }

      def sumOfTopN(ns: Seq[Int], n: Int) = ns.sorted(Ordering.Int.reverse).take(n)
    }

    val n = 3

    def run: IO[Unit] = for {
      lines <- readLines("src/main/resources/input/day_1.txt")
      elves <- Domain.parseToElfSnacks(lines)
      top   <- IO.pure(Util.sumOfTopN(elves.map(_.totalCalories), n))
      _     <- IO.println(s"Top $n: $top")
      _     <- IO.println(s"Total: ${top.sum}")
    } yield ()
  }

}
