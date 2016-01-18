import sbt._
import Keys._

object Common {
  val version       = "0.1.0"
  val scalaVersion  = "2.11.7"
  val akkaVersion   = "2.3.11"
  val organization  = "com.github.nrf110"
  val scalacOptions = Seq("-deprecation", "-encoding", "utf8")
  val dependencies  = Seq(
    "org.scala-lang"    %  "scala-library" % scalaVersion,
    "org.scala-lang"    %  "scala-reflect" % scalaVersion,
    "org.scala-lang"    %  "scalap"        % scalaVersion,
    "com.typesafe.akka" %% "akka-actor"    % akkaVersion,
    "org.scalatest"     %% "scalatest"     % "2.2.4" % "test",
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test"
  )
}
