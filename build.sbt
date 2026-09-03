scalaVersion := "3.8.4"

name := "word-usage"

Compile / run / fork := true // IOApp warning

libraryDependencies ++= Seq(
  ("com.gu" %% "content-api-client-cats-effect" % "48.0.2-PREVIEW.add-cats-effect-capi-client.2026-09-02T1126.d0c6312e").cross(CrossVersion.for3Use2_13),
  "org.apache.opennlp" % "opennlp-tools" % "2.5.11",
  "com.gu.duration-formatting" %% "core" % "0.0.2",
  "com.madgag" %% "rate-limit-status" % "1.0.1",
  "org.slf4j" % "slf4j-simple" % "2.0.18",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test
) ++ Seq("kantan.csv", "kantan.csv-java8").map(artifactId => "io.github.kantan-scala" %% artifactId % "0.11.0")