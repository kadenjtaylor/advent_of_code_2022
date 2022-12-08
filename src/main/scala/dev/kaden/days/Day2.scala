package dev.kaden.days

import dev.kaden.util.Util.Demo
import cats.effect.IO
import dev.kaden.days.Day2.Domain.*
import dev.kaden.util.Util.readLines

object Day2 extends Demo {

  object Domain {

    import Move.*
    import Input.*
    import Output.*
    import Result.*

    enum Move {
      case Rock
      case Paper
      case Scissors

      def tie = this

      def win = this match
        case Rock     => Paper
        case Paper    => Scissors
        case Scissors => Rock

      def lose = this match
        case Rock     => Scissors
        case Paper    => Rock
        case Scissors => Paper
    }

    enum Input:
      case A
      case B
      case C

    enum Output:
      case X
      case Y
      case Z

    enum Result:
      case Win
      case Loss
      case Tie

    val inputMappings = Map(
      A -> Rock,
      B -> Paper,
      C -> Scissors
    )

    val outputMappings = Map(
      X -> Rock,
      Y -> Paper,
      Z -> Scissors
    )

    val resultMappings = Map(
      X -> Loss,
      Y -> Tie,
      Z -> Win
    )

    case class Round(theirMove: Move, yourMove: Move) {
      def score: Int = {
        val yourMoveComponent = yourMove match
          case Rock     => 1
          case Paper    => 2
          case Scissors => 3
        val resultComponent = didYouWin match
          case Win  => 6
          case Loss => 0
          case Tie  => 3
        yourMoveComponent + resultComponent
      }

      def didYouWin = (theirMove, yourMove) match {
        case (a, b) if a == b  => Tie
        case (Rock, Scissors)  => Loss
        case (Paper, Rock)     => Loss
        case (Scissors, Paper) => Loss
        case _                 => Win
      }
    }

    val exampleCheatSheet = Seq(
      (A, Y),
      (B, X),
      (C, Z)
    )

    def toRoundFromMoves(tup: (Input, Output)): Option[Round] = for {
      i <- inputMappings.get(tup._1)
      o <- outputMappings.get(tup._2)
    } yield Round(i, o)

    def toRoundFromMoveAndOutcome(tup: (Input, Output)): Option[Round] = for {
      i <- inputMappings.get(tup._1)
      o <- resultMappings
        .get(tup._2)
        .map(_ match
          case Win  => i.win
          case Loss => i.lose
          case Tie  => i.tie
        )
    } yield Round(i, o)

    def parseCheatSheet(filename: String): IO[Seq[(Input, Output)]] = for {
      lines <- readLines(filename)
      tups <- IO {
        lines.map { line =>
          val chunks = line.split(" ")
          (Input.valueOf(chunks(0)), Output.valueOf(chunks(1)))
        }
      }
    } yield tups
  }

  def run: IO[Unit] = {
    import cats.Traverse.nonInheritedOps.toTraverseOps
    for {
      cheatSheet <- parseCheatSheet("src/main/resources/input/day_2.txt")
      rounds <- IO.fromOption(cheatSheet.map(toRoundFromMoveAndOutcome).sequence)(
        new RuntimeException("Your mapping sucked")
      )
      _ <- IO {
        println(s"CheatSheet Length: ${cheatSheet.size}")
        println(s"Rounds Length: ${rounds.size}")
      }
      score <- IO.pure(rounds.map(_.score).sum)
      _ <- IO {
        println(s"Your score: $score")
      }
    } yield ()
  }

}
