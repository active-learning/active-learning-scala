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

object Hits extends CrossValidation with App {
  val samplingSize = 500
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries (from hardcoded strategies) for the given list of datasets."
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val parallelStrats = args(2).contains("s")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern], f: Standardize) =>
    val nc = pool.head.nclasses

    if (checkRndQueriesAndHitsCompleteness(learner(pool.length / 2, run, pool), db, pool, run, fold, testSet, f)) {
      //para as non-Rnd strats, faz tantas matrizes de confusão quantas queries existirem na base (as matrizes são rápidas de calcular)
      val strats0 = List(
        ClusterBased(pool),
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
        ExpErrorReductionMargin(learner(pool.length / 2, run, pool), pool, "entropy", samplingSize),
        ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "accuracy", samplingSize),
        ExpErrorReduction(learner(pool.length / 2, run, pool), pool, "gmeans", samplingSize)
      )
      val strats = if (parallelStrats) strats0.par else strats0
      strats foreach { strat =>
        db.saveHits(strat, learner(pool.length / 2, run, pool), run, fold, nc, f, testSet)
      }
    }
  }
}
