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
import util.{ALDatasets, Datasets}

object Predictions extends CrossValidation with App {
  val runs = 5
  val folds = 5
  val desc = "Version " + ArgParser.version + "\n Store predictions for each run-fold.\n" +
    "The predictions are not necessarily normalized," +
    "because unnormalized values allows for 1-against-all metrics (at least for ELMs, cujas saídas são como classificadores independentes)," +
    "quando a normalização ocorre apenas entre cada par de saídas do classificador.\n"
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val fetchQueries = ALDatasets.queriesFromSQLite(path) _
  val dest = Dataset(path) _
  val samplingSize = 500

  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern]) =>
    //ignores pool
    val strats = List(
      RandomSampling(Seq()),
      ClusterBased(Seq()),
      Uncertainty(learner(1, run, Seq()), Seq()),
      Entropy(learner(1, run, Seq()), Seq()),
      Margin(learner(1, run, Seq()), Seq()),
      new SGmulti(learner(1, run, Seq()), Seq(), "consensus"),
      new SGmulti(learner(1, run, Seq()), Seq(), "majority"),
      new SGmultiJS(learner(1, run, Seq()), Seq()),
      DensityWeighted(learner(1, run, Seq()), Seq(), 1, "eucl"),
      DensityWeightedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1, "cheb"),
      DensityWeightedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1, "eucl"),
      DensityWeightedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1, "maha"),
      DensityWeightedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1, "manh"),
      MahalaWeighted(learner(1, run, Seq()), Seq(), 1),
      MahalaWeightedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1),
      ExpErrorReduction(learner(1, run, Seq()), Seq(), "entropy", samplingSize),
      ExpErrorReduction(learner(1, run, Seq()), Seq(), "accuracy", samplingSize),
      ExpErrorReduction(learner(1, run, Seq()), Seq(), "gmeans", samplingSize)
      //      MahalaWeightedRefreshed(learner(1, run, Seq()), Seq(), 1, samplingSize),
      //      MahalaWeightedRefreshedTrainingUtility(learner(1, run, Seq()), Seq(), 1, 1, samplingSize)
      //      PerfectRealisticAccuracy(learner(1, run, Seq()), Seq()),
    )

    strats foreach {
      strat =>
        //checa se as queries desse run/fold existem para o presente par strategy/learner
        if (db.isOpen && db.completePools(strat) != runs * folds) {
          println(s" ${db.completePools(strat)} $strat results incomplete. Skipping dataset $db for fold $fold of run $run.")
          //      db.close()
          //      sys.exit(0)
        } else {
          fetchQueries(db.database)(strat, run, fold) match {
            case Right(queries) =>
              val initial = queries.take(queries.head.nclasses)
              val rest = queries.drop(queries.head.nclasses)
              val model = learner(pool.length / 2, run, pool).build(initial)

              db.run("begin")
              rest map { trainingPattern =>
                val qid = trainingPattern.id
                testSet map { testingPattern =>
                  val pred = model.output(testingPattern)
                  db.run("insert into prediction values ($lid, $qid, $iid, $output, $value)")
                }
              }
              db.run("end")
            case Left(str) => println(s"Problem loading queries for $strat: $str")
          }
        }
    }
  }
}
