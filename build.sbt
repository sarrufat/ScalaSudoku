


scalaVersion := "2.11.2"

testOptions in Test += Tests.Argument("-oD")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"