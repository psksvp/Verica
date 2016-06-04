name := "Verica"
version := "0.1"
scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
"org.scalatest" % "scalatest_2.11" % "2.2.0" % "test" withSources(),
"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1" withSources()
)
libraryDependencies += "ch.qos.logback" %  "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"