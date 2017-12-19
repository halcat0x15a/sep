lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.halcat",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT",
    )),
    name := "sep",
    fork in run := true,
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-ahc-ws" % "2.6.9",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
    )
  )
