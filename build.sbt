name := """apex-schema"""

version := "1.0.0"

organization := "com.systematicmethods"

scalaVersion := "2.11.7"

resolvers += Resolver.mavenLocal

val OrientDBVersion = "2.2.0-beta2"
val SparkVersion = "1.6.1"
    
libraryDependencies ++= Seq(
  "org.apache.avro" % "avro" % "1.8.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.orientechnologies" % "orientdb-core" % OrientDBVersion,
  "com.orientechnologies" % "orientdb-object" % OrientDBVersion,
  "com.orientechnologies" % "orientdb-graphdb" % OrientDBVersion,
  "com.orientechnologies" % "orientdb-client" % OrientDBVersion,
  "com.opencsv" % "opencsv" % "3.3",
  "org.apache.commons" % "commons-csv" % "1.2",
  "org.apache.spark" %% "spark-core" % SparkVersion,
  "org.apache.spark" %% "spark-sql" % SparkVersion,
  "org.apache.spark" %% "spark-streaming" % SparkVersion,
  "org.apache.spark" %% "spark-mllib" % SparkVersion,
  "com.databricks" %% "spark-csv" % "1.4.0",
  "com.typesafe.play" %% "play-json" % "2.5.2",


// internal

  // test
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "org.scalatest" %% "scalatest" % "2.2.5" % "test"
)




