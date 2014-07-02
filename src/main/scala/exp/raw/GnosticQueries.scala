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

/**
 * Created by davi on 05/06/14.
 */
object GnosticQueries extends CrossValidation with App {
  val runs = 5
  val folds = 5
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded GNOSTIC " +
    "strategies (i.e. not Rnd and Clu) mostly due to the fact that they can be stopped earlier when a predefined Q is given;\n"
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _
  val samplingSize = 1000

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern]) =>
    val strats = List(
      //      PerfectRealisticAccuracy(learner(pool.length / 2, run, pool), pool),
      Uncertainty(learner(pool.length / 2, run, pool), pool),
      Entropy(learner(pool.length / 2, run, pool), pool),
      Margin(learner(pool.length / 2, run, pool), pool),
      new SGmulti(learner(pool.length / 2, run, pool), pool, "consensus"),
      new SGmulti(learner(pool.length / 2, run, pool), pool, "majority"),
      new SGmultiJS(learner(pool.length / 2, run, pool), pool),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "entropy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "accuracy", samplingSize),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "cheb"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "eucl"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "maha"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "manh"),
      MahalaWeighted(learner(pool.length / 2, run, pool), pool, 1),
      MahalaWeightedRefreshed(learner(pool.length / 2, run, pool), pool, 1, samplingSize),
      MahalaWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1),
      MahalaWeightedRefreshedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, samplingSize)
    )

    //checa se as queries desse run/fold existem para Random/NoLearner

    ??? //de onde tirar o Q de cada dataset?
    strats foreach (strat => db.saveQueries(strat, run, fold, Q))
  }

}


//def learner(Lmax: Int, seed: Int) =
////      C45()
//NB()
////      interaELM(Lmax, seed)

