import sbt._
import Keys._

object MyBuild extends Build {

//  lazy val root = Project("root", file(".")) dependsOn(elmsProj)
//  lazy val elmsProj = RootProject(uri("https://github.com/extreme-learning-machine/elm-scala.git"))


  lazy val root = Project("root", file("."))
//  .aggregate(p1,p2)
                    .dependsOn(p1)
                    .dependsOn(p2)
  lazy val p1 = RootProject(uri("https://github.com/machine-learning-scala/mls.git"))
  lazy val p2   = RootProject(uri("https://github.com/extreme-learning-machine/elm-scala.git"))

//  lazy val p1 = project.in(uri("https://github.com/machine-learning-scala/mls.git"))
//  lazy val p2 = project in uri("https://github.com/extreme-learning-machine/elm-scala.git")

}
