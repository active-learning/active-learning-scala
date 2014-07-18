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

/**
 * Created by davi on 05/06/14.
 */
object LightGnosticQueries extends CrossValidation with App {
  val runs = 5
  val folds = 5
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded GNOSTIC " +
    "strategies (i.e. not Rnd and Clu) mostly due to the fact that they can be stopped earlier when a predefined Q is given;\n"
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
      Uncertainty(learner(pool.length / 2, run, pool), pool),
      Entropy(learner(pool.length / 2, run, pool), pool),
      Margin(learner(pool.length / 2, run, pool), pool),
      new SGmulti(learner(pool.length / 2, run, pool), pool, "consensus"),
      new SGmulti(learner(pool.length / 2, run, pool), pool, "majority"),
      new SGmultiJS(learner(pool.length / 2, run, pool), pool),
      DensityWeighted(learner(pool.length / 2, run, pool), pool, 1, "eucl"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "cheb"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "eucl"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "maha"),
      DensityWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, "manh"),
      MahalaWeighted(learner(pool.length / 2, run, pool), pool, 1),
      MahalaWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "entropy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "accuracy", samplingSize),
      ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "gmeans", samplingSize)
      //      MahalaWeightedRefreshed(learner(pool.length / 2, run, pool), pool, 1, samplingSize),
      //      MahalaWeightedRefreshedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1, samplingSize)
      //      PerfectRealisticAccuracy(learner(pool.length / 2, run, pool), pool),
    ).reverse
    val strats = if (parallelStrats) strats0.par else strats0

    //checa se as queries desse run/fold existem para Random/NoLearner
    if (db.isOpen && db.rndCompletePools != runs * folds) {
      println(s" ${db.rndCompletePools} Random Sampling results incomplete. Skipping dataset $db for fold $fold of run $run.")
      //      db.close()
      //      sys.exit(0)
    } else {
      //de onde tirar o Q de cada dataset? limitar por tempo!
      strats foreach (strat => db.saveQueries(strat, run, fold, 8000))
    }

    /*
    ~200 datasets
    25 pools
    ~5 slow strategies
    total: 25000
    avg time for cluster-based in feasible 169 datasets: 182s
    worst time in feasible 169 datasets: 2h

    other datasets (require more memory = less threads): 35
    total number of pools: 35 * 25 * 5 = 4375
    estimated time per pool: 4h
    estimated total CPU time: 4 * 4375 = 17500h = 24meses
    estimated total wall time: 24meses / (2servers * 2threads) = 6 meses

    available time: 3meses * 2servers * 2threads = ~8000h
    available time limit per pool: 8000 / 4375 = ~2h

    time limit chosen (only 2 strats are really slow): 4h
     */
  }

}
