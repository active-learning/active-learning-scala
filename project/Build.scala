import sbt._

object MyBuild extends Build {

  //  lazy val root = Project("root", file(".")) dependsOn(elmsProj)
  //  lazy val elmsProj = RootProject(uri("https://github.com/extreme-learning-machine/elm-scala.git"))

  lazy val root = Project("root", file("."))
                    .dependsOn(p1)
                    .dependsOn(p2)

  lazy val p1 = RootProject(uri("https://github.com/machine-learning-scala/mls.git"))
  lazy val p2   = RootProject(uri("https://github.com/extreme-learning-machine/elm-scala.git"))

}
