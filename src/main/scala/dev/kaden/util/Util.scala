package dev.kaden.util

import cats.effect.IO

object Util {

  trait Demo {
    def run: IO[Unit]
  }

  def readLines(filename: String): IO[Seq[String]] = {
    import scala.io.Source
    IO {
      Source.fromFile(filename).getLines().toSeq
    }
  }

}
