name := "SimulatedAnnealing"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds", "-language:postfixOps")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.11" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "org.scalaz" %% "scalaz-effect" % "7.1.0"
)

resolvers ++= Seq("releases"  at "http://oss.sonatype.org/content/repositories/releases")
