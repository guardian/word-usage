scalaVersion := "3.3.7"

name := "word-usage"

Compile / run / fork := true // IOApp warning

libraryDependencies ++= Seq(
  ("com.gu" %% "content-api-client-default" % "37.1.0").cross(CrossVersion.for3Use2_13),
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "org.apache.opennlp" % "opennlp-tools" % "2.5.6.1",
  "org.typelevel" %% "cats-effect" % "3.6.3",
  "com.github.cb372" %% "cats-retry" % "4.0.0",
  "co.fs2" %% "fs2-io" % "3.12.2",
  "com.gu.duration-formatting" %% "core" % "0.0.2",
  "com.softwaremill.sttp.client4" %% "cats" % "4.0.12",
  "com.madgag" %% "rate-limit-status" % "1.0.1",
  "org.slf4j" % "slf4j-simple" % "2.0.17",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
) ++ Seq("kantan.csv", "kantan.csv-java8").map(artifactId => "io.github.kantan-scala" %% artifactId % "0.11.0")