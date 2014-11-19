name := "als"

version := "81"

/*
0 rnd
1 clu
2 unc
3 mar
4 ent
5 DW
6 DWTUeu
7 DWTUman
8 DWTUche
9 DWTUmah
10
11 eer ent
12 eer acc
13 eer gme
14 sg con
15 sg maj
17 simple
18 self
19 kff
20 balanced

36 dwlau eu

46 dwlou eu
 */

scalaVersion := "2.10.4"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.11"

//libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.15-M1"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.33"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.2"

//libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.1"

//libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"


//scalaSource in Compile := baseDirectory.value / "src"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:reflectiveCalls")


