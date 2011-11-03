name := "json"

version := "0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
//  "-Xlog-implicits"
  "-unchecked"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.0" % "test",
  "com.novocode" % "junit-interface" % "0.7" % "test->default"
)

