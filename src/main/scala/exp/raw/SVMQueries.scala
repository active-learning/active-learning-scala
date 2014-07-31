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

package exp.raw

import al.strategies._
import app.ArgParser
import app.db.Dataset
import ml.Pattern
import ml.classifiers.LASVM
import util.Datasets
import weka.filters.unsupervised.attribute.Standardize

object SVMQueries extends CrossValidation with App {
  val samplingSize = 500
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded SVM strategies \n"
  val (path, datasetNames) = ArgParser.testArgs(className, args, 3, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val parallelStrats = args(2).contains("s")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern], f: Standardize) =>
    val strats0 = List(
      SVMmulti(pool, "SELF_CONF"),
      SVMmulti(pool, "KFF"),
      SVMmulti(pool, "BALANCED_EE"),
      SVMmulti(pool, "SIMPLE")
    )
    val strats = if (parallelStrats) strats0.par else strats0
    if (checkRndQueriesAndHitsCompleteness(LASVM(), db, pool, run, fold, testSet, f)) {
      val Q = q(db, LASVM())
      strats foreach { strat =>
        println(s"Strat: $strat")
        db.saveQueries(strat, run, fold, f, timeLimitSeconds, Q)
      }
    }
  }
}
