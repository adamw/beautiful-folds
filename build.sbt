organization  := "com.softwaremill"

name := "beautiful-folds"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  //"com.twitter" %% "algebird-core" % "0.12.2"
  "org.typelevel" %% "cats" % "0.8.0",
  "com.github.julien-truffaut" %%  "monocle-core" % "1.3.2"
)
