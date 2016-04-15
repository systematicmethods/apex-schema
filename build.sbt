name := """apex-schema"""

version := "1.0.0"

organization := "com.systematicmethods"

scalaVersion := "2.11.7"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "org.apache.avro" % "avro" % "1.8.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.orientechnologies" % "orientdb-core" % "2.2.0-beta2",
  "com.orientechnologies" % "orientdb-object" % "2.2.0-beta2",
  "com.orientechnologies" % "orientdb-graphdb" % "2.2.0-beta2",
  "com.orientechnologies" % "orientdb-client" % "2.2.0-beta2",
//  "com.github.melrief" %% "purecsv" % "0.0.6",
//  "com.nrinaudo" %% "kantan.csv" % "0.1.8",
//  "com.nrinaudo" %% "kantan.csv-generic" % "0.1.8",
//  "com.nrinaudo" %% "kantan.csv-scalaz" % "0.1.8",
//  "com.nrinaudo" %% "kantan.csv-scalaz-stream" % "0.1.8",
//  "com.nrinaudo" %% "kantan.csv-cats" % "0.1.8",
  "com.opencsv" % "opencsv" % "3.3",
  "org.apache.commons" % "commons-csv" % "1.2",
//  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.apache.spark" %% "spark-core" % "1.6.1",
  "org.apache.spark" %% "spark-sql" % "1.6.1",
  "org.apache.spark" %% "spark-streaming" % "1.6.1",
  "org.apache.spark" %% "spark-mllib" % "1.6.1",
  "com.databricks" %% "spark-csv" % "1.4.0",

// internal

  // test
  "junit" % "junit" % "4.12" % "test",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test"  
)




