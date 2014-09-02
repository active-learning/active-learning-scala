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
import ml.classifiers.NB
import weka.filters.unsupervised.attribute.Standardize

object RandomNB extends CrossValidation with App {
  val args1 = args
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries (from hardcoded rnd strategy) for the given list of datasets."
  val (path, datasetNames0) = ArgParser.testArgs(className, args, 3, desc)

  run(ff)

  def strats0(run: Int, pool: Seq[Pattern]) = List(RandomSampling(Seq()))

  def ee(db: Dataset) = {
    val fazer = !db.isLocked() && (if (!rndQueriesComplete(db)) {
      println(s"Rnd queries are incomplete for $db. Skipping...")
      false
    } else {
      if (!db.rndHitsComplete(NB())) true
      else {
        println(s"Rnd NB hits are complete for $db.")
        false
      }
    })
    fazer
  }

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize) {
    val nc = pool.head.nclasses

    //Completa NB hits do Rnd
    val Q = 10000
    strats(run, pool).takeWhile { s =>
      db.saveHits(s, NB(), run, fold, nc, f, testSet, 8 * 3600, Q)
      db.running
    }
  }
}
