name := "als"

version := "11+knn+vfdt+svm+mahs+c45+i"

scalaVersion := "2.10.4"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.11"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.15-M1"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.2"

//libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.1"

//libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"


//scalaSource in Compile := baseDirectory.value / "src"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:reflectiveCalls")


