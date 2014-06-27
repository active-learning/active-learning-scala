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
import app.db.Dataset
import app.ArgParser
import ml.Pattern
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.PerfectAccuracy
import al.strategies.MahalaWeightedTrainingUtility
import app.db.Dataset
import al.strategies.MahalaWeightedRefreshed
import util.Datasets

/**
 * Created by davi on 05/06/14.
 */
object GnosticQueries extends CrossValidation with App {
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded GNOSTIC " +
    "strategies (i.e. not Rnd and Clu) mostly due to the fact that they can be stopped earlier when a predefined Q is given;\n" +
    "Parallel means 'to parallelize datasets, but serialize runs and folds."
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(getClass.getSimpleName.dropRight(1), args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _
  val samplingSize = 500

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern]) {
    val strats = List(
      PerfectRealisticAccuracy(learner(pool.length / 2, run), pool),
      Uncertainty(learner(pool.length / 2, run), pool),
      Entropy(learner(pool.length / 2, run), pool),
      Margin(learner(pool.length / 2, run), pool),
      new SGmulti(learner(pool.length / 2, run), pool, "consensus"),
      new SGmulti(learner(pool.length / 2, run), pool, "majority"),
      new SGmultiJS(learner(pool.length / 2, run), pool),
      ExpErrorReduction(learner(pool.length / 2, run), pool, "entropy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run), pool, "accuracy", samplingSize),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1, "cheb"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1, "eucl"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1, "maha"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1, "manh"),
      MahalaWeighted(learner(pool.length / 2, run), pool, 1),
      MahalaWeightedRefreshed(learner(pool.length / 2, run), pool, 1, samplingSize),
      MahalaWeightedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1),
      MahalaWeightedRefreshedTrainingUtility(learner(pool.length / 2, run), pool, 1, 1, samplingSize)
    )
    ??? //de onde tirar o Q de cada dataset?
//    strats foreach (strat => db.writeQueries(strat, run, fold, Q))
  }

  run
}


//def learner(Lmax: Int, seed: Int) =
////      C45()
//NB()
////      interaELM(Lmax, seed)

