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
import ml.classifiers._
import weka.filters.unsupervised.attribute.Standardize

object SVM extends CrossValidation with App {
  lazy val binarizeNominalAtts = !SVMLib().toString.contains("semzscore")
  val args1 = args
  val desc = "Version " + ArgParser.version + " \n Generates confusion matrices for queries (from hardcoded SVM strategies) for the given list of datasets. Uses weka despite the queries coming from another SVM implementation."
  val (path, datasetNames0) = ArgParser.testArgs(className, args, 3, desc)

  run(ff)

  //para as non-Rnd strats, faz tantas matrizes de confusão quantas queries existirem na base (as matrizes são rápidas de calcular, espero)
  def strats0(run: Int, pool: Seq[Pattern]) = List(
    SVMmulti(pool, "SELF_CONF"),
    SVMmulti(pool, "KFF"),
    SVMmulti(pool, "BALANCED_EE"),
    SVMmulti(pool, "SIMPLE")
  )

  def ee(db: Dataset) = {
    val fazer = !db.isLocked() && (if (!db.rndHitsComplete(NB()) || !db.rndHitsComplete(KNNBatch(5, "eucl", Seq(), "", weighted = true)) || !db.rndHitsComplete(C45())) {
      println(s"Rnd NB or 5NN or C45 hits are incomplete for $db with ${SVMLib()}. Skipping...")
      false
    } else {
      if (!hitsComplete(SVMLib())(db)) {
        if (nonRndQueriesComplete(db)) true
        else {
          println(s"SVM queries are incomplete for $db for some of the given strategies. Skipping...")
          false
        }
      } else {
        println(s"SVM hits are complete for $db with ${SVMLib()}.")
        false
      }
    })
    fazer
  }

  def ff(db: Dataset, run: Int, fold: Int, pool: => Seq[Pattern], testSet: => Seq[Pattern], f: => Standardize, pattsFromARFFMap: => Map[Int, Pattern]) {
    val nc = pool.head.nclasses
    val Q = q(db)
    strats(run, pool).takeWhile { s =>
      db.saveHits(s, SVMLib(run * 100 + fold), run, fold, nc, f, testSet, timeLimitSeconds, pattsFromARFFMap, Q)
      db.running
    }
  }
}
