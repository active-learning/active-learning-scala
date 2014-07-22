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

import al.strategies.RandomSampling
import app.ArgParser
import app.db.Dataset
import ml.Pattern
import util.Datasets
import weka.filters.unsupervised.attribute.Standardize

/**
 * Created by davi on 05/06/14.
 */
object RandomHits extends CrossValidation with App {
  val runs = 5
  val folds = 5
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries for the given list of datasets according to Random Sampling " +
    "Rnd because it is the baseline to define Q."
  val (path, datasetNames, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val parallelDatasets = args(2).contains("d")
  val parallelRuns = args(2).contains("r")
  val parallelFolds = args(2).contains("f")
  val source = Datasets.patternsFromSQLite(path) _
  val dest = Dataset(path) _
  run { (db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: Seq[Pattern], f: Standardize) =>
    val nc = pool.head.nclasses

    //checa se as queries desse run/fold existem para Random/NoLearner
    if (db.isOpen && db.rndCompletePools != runs * folds) {
      println(s" ${db.rndCompletePools} Random Sampling results incomplete. Skipping dataset $db for fold $fold of run $run.")
    } else {

      //checa se tabela de matrizes de confusão está completa para Random/learner
      val n = pool.length * pool.head.nclasses * pool.head.nclasses
      val nn = db.rndCompleteHits(RandomSampling(Seq()), learner(pool.length / 2, run, pool), run, fold)
      if (nn > n) println(s"$nn hits should be lesser than $n for run $run fold $fold for $db")
      else if (nn < n) db.saveHits(RandomSampling(Seq()), learner(pool.length / 2, run, pool), run, fold, nc)
    }
  }
}
