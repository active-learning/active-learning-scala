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

package exp.hit

import al.strategies._
import app.ArgParser
import app.db.entities.Dataset
import exp.CrossValidation
import ml.Pattern
import ml.classifiers.{C45, KNNBatch, NB}
import weka.filters.unsupervised.attribute.Standardize

object Random extends CrossValidation with App {
  lazy val binarizeNominalAtts = !learner(-1, Seq()).toString.contains("semzscore")
  val args1 = args
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries (from hardcoded RandomSampling/learner) for the given list of datasets."
  val (path, datasetNames0, learner) = ArgParser.testArgsWithLearner(className, args, desc)

  run(ff)

  def strats0(run: Int, pool: Seq[Pattern]) = List(
    RandomSampling(pool)
  )

  def ee(db: Dataset) = {
    val fazer = !db.isLocked() && (if (!db.rndHitsComplete(NB()) || !db.rndHitsComplete(KNNBatch(5, "eucl", Seq(), "", weighted = true)) || !db.rndHitsComplete(C45())) {
      println(s"Rnd NB, 5NN or C45 hits are incomplete for $db with ${learner(-1, Seq())}. Skipping...")
      false
    } else {
      if (!hitsComplete(learner(-1, Seq()))(db)) {
        if (nonRndQueriesComplete(db)) true
        else {
          println(s"Queries are incomplete for $db for some of the given strategies. Skipping...")
          false
        }
      } else {
        println(s"Rnd hits are complete for $db with ${learner(-1, Seq())}.")
        false
      }
    })
    fazer
  }

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize, pattsFromARFFMap: => Map[Int, Pattern]) {
    val nc = pool.head.nclasses
    val Q = q(db)
    strats(run, pool).takeWhile { s =>
      db.saveHits(s, learner(run, pool), run, fold, nc, f, testSet, timeLimitSeconds, pattsFromARFFMap, Q)
      db.running
    }
  }
}