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
import util.Datasets
import weka.filters.unsupervised.attribute.Standardize

object HeavyGnosticQueries extends CrossValidation with App {
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded heavy GNOSTIC " +
    "strategies (EER entr, acc and gmeans) mostly due to the fact that they are slow and are stopped by time limit of 8000s;\n"
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val parallelStrats = args(2).contains("s")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _
  val samplingSize = 500

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern], f: Standardize) =>
    val strats0 = List(
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "entropy", samplingSize),
      ExpErrorReductionMargin(learner(pool.length / 2, run, pool), pool, "entropy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "accuracy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "gmeans", samplingSize)
      //      MahalaWeightedRefreshed(learner(pool.length / 2, run, pool), pool, 1, samplingSize),
      //      MahalaWeightedRefreshedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, samplingSize) //Too slow
      //      PerfectRealisticAccuracy(learner(pool.length / 2, run, pool), pool), //useless
    )
    val strats = if (parallelStrats) strats0.par else strats0
    if (checkRndQueriesAndHitsCompleteness(learner(pool.length / 2, run, pool), db, pool, run, fold, testSet, f)) {
      val Q = q(db, learner(pool.length / 2, run, pool))
      strats foreach (strat => db.saveQueries(strat, run, fold, f, timeLimitSeconds, Q))
    }
  }
}
