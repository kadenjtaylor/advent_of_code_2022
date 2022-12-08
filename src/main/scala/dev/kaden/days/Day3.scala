package dev.kaden.days

import dev.kaden.util.Util.Demo
import dev.kaden.days.Day3.Domain.Item.*
import cats.effect.IO
import dev.kaden.days.Day3.Domain.Item
import dev.kaden.days.Day3.Domain.*
import dev.kaden.days.Day3.Domain.Error.*

object Day3 extends Demo {

  object Domain {

    enum Item {
      case a
      case b
      case c
      case d
      case e
      case f
      case g
      case h
      case i
      case j
      case k
      case l
      case m
      case n
      case o
      case p
      case q
      case r
      case s
      case t
      case u
      case v
      case w
      case x
      case y
      case z
      case A
      case B
      case C
      case D
      case E
      case F
      case G
      case H
      case I
      case J
      case K
      case L
      case M
      case N
      case O
      case P
      case Q
      case R
      case S
      case T
      case U
      case V
      case W
      case X
      case Y
      case Z

      def priority = this.ordinal + 1
    }

    enum Error extends Throwable:
      case RucksackSizeImbalance(s1: Int, s2: Int)

    case class Rucksack private (s1: Seq[Item], s2: Seq[Item]) {
      def commonElements() = {
        s1.intersect(s2)
      }
      override def toString(): String = {
        s"{ 1: ${s1.mkString}, 2: ${s2.mkString} }"
      }
    }
    object Rucksack {
      def apply(contents: Seq[Item]): Either[Error, Rucksack] = {
        val half   = contents.size / 2
        val whole  = contents.size
        val first  = contents.slice(0, half)
        val second = contents.slice(half, whole)
        if (first.size == second.size) {
          Right(Rucksack(first, second))
        } else {
          Left(RucksackSizeImbalance(first.size, second.size))
        }
      }
    }

    def parseToRuckSack(line: String): IO[Rucksack] = for {
      sack <- IO.fromEither(Rucksack(line.map(s => Item.valueOf(s.toString()))))
    } yield sack

    def commonToAll(s: Seq[Rucksack]): Seq[Item] = {
      s.foldLeft(Seq[Item]()) {
        case (Seq(), sack) => sack.s1 ++ sack.s2
        case (acc, sack)   => acc.intersect(sack.s1 ++ sack.s2)
      }.distinct
    }

    def detectBadge(s: Seq[Rucksack]) = commonToAll(s) match {
      case Seq(a) => Some(a)
      case _      => None
    }

    val exampleData =
      """vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  }

  import cats.implicits.toTraverseOps
  import dev.kaden.util.Util.readLines

  def run: IO[Unit] = for {
    // lines <- IO.pure(exampleData.split("\n").toSeq)
    lines   <- readLines("src/main/resources/input/day_3.txt")
    sacks   <- lines.map(parseToRuckSack(_)).sequence
    triples <- IO.pure(sacks.grouped(3).toSeq)
    _       <- IO.println(s"lines: ${lines.size}")
    _       <- IO.println(s"sacks: ${sacks.size}")
    _       <- IO.println(s"triples: ${triples.size}")
    commons <- IO.pure(triples.map(detectBadge(_)))
    _       <- IO.println(commons.size)
    prios   <- IO.pure(commons.flatten.map(_.priority))
    _       <- IO.println(prios.size)
    _       <- IO.println(prios.sum)
  } yield ()

}
