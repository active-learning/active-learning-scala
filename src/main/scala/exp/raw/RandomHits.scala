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
import ml.classifiers.{NoLearner, NB}
import util.Datasets
import weka.filters.unsupervised.attribute.Standardize

object RandomHits extends CrossValidation with App {
  val args1 = args
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries (from hardcoded rnd strategy) for the given list of datasets."
  val (path, datasetNames0, learner) = ArgParser.testArgsWithLearner(className, args, desc)
  val datasetNames = datasetNames0.filter { d =>
    val db = Dataset(path, createOnAbsence = false, readOnly = true)(d)
    val res = rndQueriesComplete(db) && (!rndNBHitsComplete(db) || !hitsComplete(learner(-1, -1, Seq()), db)(d))
    db.close()
    res
  }
  run(ff)

  def strats0(run: Int, pool: Seq[Pattern]) = List(RandomSampling(Seq()))

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize) {
    val nc = pool.head.nclasses

    //Completa hits do Rnd
    strats(run, pool).foreach(s => db.saveHits(s, NB(), run, fold, nc, f, testSet, 2 * 3600))

    //Verifica se passo anterior antingiu |Pool|.
    if (rndNBHitsComplete(db)) {
      val Q = q_notCheckedIfHasAllRndQueries(db)

      //Retoma Rnd Hits para o dado learner como arg na linha de comando, limitando por tempo e Q.
      strats(run, pool).foreach(s => db.saveHits(s, learner(pool.length / 2, run, pool), run, fold, nc, f, testSet, timeLimitSeconds, Q))
    } else println(s"Rnd NB hits still incomplete! Skipping ${learner(-1, -1, Seq())} hits.")
  }
}
