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
  val desc = "Version " + ArgParser.version + "\n Generates queries for the given list of datasets according to provided hardcoded light GNOSTIC " +
    "strategies (i.e. not Rnd, Clu and not EER) mostly due to the fact that they are fast and don't need to be stopped earlier;\n"
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
      MahalaWeightedTrainingUtility(learner(pool.length / 2, run, pool), pool, 1, 1)
    )
    val strats = if (parallelStrats) strats0.par else strats0

    //checa se as queries desse run/fold existem para Random/NoLearner
    if (db.isOpen && db.rndCompletePools != runs * folds) {
      println(s" ${db.rndCompletePools} Random Sampling results incomplete. Skipping dataset $db for fold $fold of run $run.")
      //      db.close()
      //      sys.exit(0)
    } else {
      //checa se tabela de matrizes de confusão existe para Random/learner
      val n = pool.length * pool.head.nclasses * pool.head.nclasses
      val nn = db.rndCompleteHits(RandomSampling(Seq()), learner(pool.length / 2, run, pool), run, fold)
      if (nn != n) {
        println(s"$nn hits should be $n for run $run fold $fold for $db")
      } else {
        //calcula Q (média de queries necessárias para Rnd atingir acurácia máxima)
        ???

        strats foreach (strat => db.saveQueries(strat, run, fold, 21600))
      }
    }
  }

}
