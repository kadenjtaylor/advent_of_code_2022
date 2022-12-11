package dev.kaden.util

import cats.effect.IO
import fs2.Stream
import fs2.io.file.{Files, Path}

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

  def streamLines(filename: String): Stream[IO, String] =
    Files[IO].readUtf8Lines(Path(filename))

}
