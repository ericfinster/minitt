val commonSettings = Seq(
  organization := "opetopic",
  homepage := Some(url("http://ericfinster.github.io")),
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    "-feature",
    "-deprecation"
  ),
  initialCommands in console := 
    """
       import minitt._
    """
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "minitt",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scalaz" %% "scalaz-core" % "7.1.3"
    )
  )



