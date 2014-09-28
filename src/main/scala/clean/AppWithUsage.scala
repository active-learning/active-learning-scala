package clean

/*
active-learning-scala: Active Learning library for Scala
Copyright (c) 2014 Davi Pereira dos Santos

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

trait AppWithUsage extends App with Log with ArgParser {
  Class.forName("org.sqlite.JDBC")
  val superArguments = List("debug-intensity:0,1,...", "datasets-path", "files-with-dataset-names:file1,file2", "paralleliz(runs folds):r|f|rf")
  val arguments: List[String]
  lazy val debugIntensity = args(0)
  lazy val path = args(1)
  lazy val datasets = datasetsFromFiles(args(2))
  lazy val parallelRuns = args(3).contains("r")
  lazy val parallelFolds = args(3).contains("f")
  lazy val learnerStr = args(4)

  def init() {
    println(args.mkString(" "))
    if (args.size != arguments.size) {
      println(s"Usage: java -cp your-path/als-version.jar ${this.getClass.getCanonicalName.dropRight(1)} ${arguments.mkString(" ")}")
      sys.exit(1)
    }
  }
}
