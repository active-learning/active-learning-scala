name := "als"

version := "1.1"

scalaVersion := "2.10.4"

//resolvers ++= Seq(
//  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
//  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
//)

//resolvers ++= Seq(Opts.resolver.sonatypeReleases)

//libraryDependencies  ++= Seq(
            // other dependencies here
//            "org.scalanlp" %% "breeze" % "0.8.1",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
//            "org.scalanlp" %% "breeze-natives" % "0.8.1"
//)

//resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.8-SNAPSHOT), use this.
//            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
//            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
//)

//libraryDependencies  ++= Seq(
//  "org.scalanlp" % "breeze_2.10" % "0.8-SNAPSHOT",
//  "org.scalanlp" % "breeze-natives_2.10" % "0.8-SNAPSHOT"
//)

//libraryDependencies += "fi.reaktor" %% "sqltyped" % "0.4.0"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.11"

//libraryDependencies += "org.encog" % "encog-core" % "3.1.0"

//libraryDependencies += "org.scala-saddle" %% "saddle-core" % "1.3.+"

libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.1"

//libraryDependencies +=  "org.scalanlp" % "breeze_2.10" % "0.6.1"

//libraryDependencies += "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.24"

//libraryDependencies += "com.typesafe.slick" % "slick_2.10" % "2.1.0-M1"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.15-M1"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

//libraryDependencies += "org.hsqldb" % "hsqldb" % "2.3.2"

//libraryDependencies += "com.h2database" % "h2" % "1.4.178"

//scalaSource in Compile := baseDirectory.value / "src"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:reflectiveCalls")
