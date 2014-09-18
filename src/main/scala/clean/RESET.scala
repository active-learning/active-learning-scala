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

package clean

import al.strategies.{MahalaWeightedTrainingUtility, DensityWeightedTrainingUtility, Strategy}
import clean.agno._
import ml.Pattern
import ml.classifiers._
import ml.neural.elm.ELM
import util.Datasets

import scala.io.Source
import scala.util.Random

trait RESET extends AppWithUsage {
  val arguments = List("datasets-path", "file-with-dataset-names", "paralleliz(runs folds):r|f|rf")
  lazy val parallelRuns = args(2).contains("r")
  lazy val parallelFolds = args(2).contains("f")
  val context = "RESETapp"
  lazy val path = args(0)
  lazy val datasets = Source.fromFile(args(1)).getLines().filter(_.length > 2).filter(!_.startsWith("#"))
  init()

  override def init() {
    super.init()
    datasets foreach { dataset =>
      val ds = Ds(path, dataset)
      ds.open()
      ds.reset()
      ds.log(s"$dataset resetado.", 2)
    }
    log("Datasets zerados.")
  }
}
