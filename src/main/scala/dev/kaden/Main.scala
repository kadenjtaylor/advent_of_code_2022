package dev.kaden

import cats.effect.IOApp
import cats.effect.IO

// Logging imports
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.LoggerFactory
import dev.kaden.util.Logging.logging

import dev.kaden.days.Day1
import dev.kaden.days.Day2
import dev.kaden.days.Day3

object Main extends IOApp.Simple {

  val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

  // This is your new "main"!
  def run: IO[Unit] = for {
    greeting <- Day3.run
  } yield ()
}
