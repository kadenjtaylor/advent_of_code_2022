lazy val root = (project in file(".")).settings(
  name         := "advent_of_code_2022",
  organization := "dev.kaden",
  scalaVersion := "3.2.1",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.4.2",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.4.2",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.4.2",
    // STREAMMMSSSS
    "co.fs2" %% "fs2-core" % "3.4.0",
    // Streams IO Stuff
    "co.fs2" %% "fs2-io" % "3.4.0",
    // Effectful testing via Weaver
    "com.disneystreaming" %% "weaver-cats" % "0.7.6" % Test
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  // Effectful logging via Log4Cats -> SLF4J -> Logback
  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.6",
    "org.typelevel" %% "log4cats-slf4j"  % "2.4.0"
  )
)
