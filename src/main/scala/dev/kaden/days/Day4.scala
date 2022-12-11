package dev.kaden.days

import dev.kaden.util.Util.Demo
import cats.effect.IO
import dev.kaden.util.Util.streamLines
import fs2.Stream
import scala.runtime.RangedProxy

object Day4 extends Demo {

  type RangePair = (Range, Range)

  val exampleData: Stream[IO, (Range, Range)] = Stream.emits(
    Seq(
      (Range(2, 4), Range(6, 8)),
      (Range(2, 3), Range(4, 5)),
      (Range(5, 7), Range(7, 9)),
      (Range(2, 8), Range(3, 7)),
      (Range(6, 6), Range(4, 6)),
      (Range(2, 6), Range(4, 8))
    )
  )

  val parsedPairs = parsePairsOfRanges("src/main/resources/input/day_4.txt")

  private def parseRangePair(line: String): IO[RangePair] = for {
    rangePair <- IO {
      line
        .split(",")
        .flatMap(_.split("-"))
        .map(Integer.parseInt(_))
    }.map(arr => (Range(arr(0), arr(1)), Range(arr(2), arr(3))))
  } yield rangePair

  def parsePairsOfRanges(filename: String): Stream[IO, RangePair] =
    streamLines(filename).flatMap(line => Stream.eval(parseRangePair(line)))

  def subsumptions(pairs: Stream[IO, RangePair]) =
    pairs.filter((a, b) => a.fullyContains(b) || b.fullyContains(a))

  def overlaps(pairs: Stream[IO, RangePair]) =
    pairs.filter(_.overlaps(_))

  def nonZeroCoverage(pairs: Stream[IO, RangePair]) =
    pairs.filter((a, b) => a.coverage(b) > 0)

  def fullSuite(pairs: Stream[IO, RangePair]) = for {
    subsumptions    <- subsumptions(pairs).compile.count
    _               <- IO.println(s"Found $subsumptions subsumptions!")
    overlaps        <- overlaps(pairs).compile.count
    _               <- IO.println(s"Found $overlaps overlaps!")
    nonZeroCoverage <- nonZeroCoverage(pairs).compile.count
    _               <- IO.println(s"Found $nonZeroCoverage pairs with non-zero coverage!")
  } yield ()

  override def run: IO[Unit] = fullSuite(parsedPairs)

  case class Range(start: Int, end: Int) {
    def fullyContains(other: Range): Boolean = {
      start <= other.start && end >= other.end
    }
    def covers(i: Int): Boolean = {
      start <= i && i <= end
    }
    def overlaps(other: Range) =
      (start <= other.start && other.start <= end) ||
        (other.start <= start && start <= other.end)

    def coverage(other: Range): Int = {
      val min = Math.min(other.start, start)
      val max = Math.max(other.end, end)
      val coverage = for {
        r <- min to max
      } yield (other.covers(r) && covers(r))
      coverage.count(identity)
    }
  }

}
