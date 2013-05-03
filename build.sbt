name := "SimulatedAnnealing"

version := "1.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds", "-language:postfixOps")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" % "test",
    "org.scalaz" %% "scalaz-core" % "7.0.0",
    "org.scalaz" %% "scalaz-concurrent" % "7.0.0",
    "org.scalaz" %% "scalaz-effect" % "7.0.0",
    "org.scalaz" %% "scalaz-iteratee" % "7.0.0"
)

resolvers ++= Seq("releases"  at "http://oss.sonatype.org/content/repositories/releases")
