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

package al.strategies

import ml.Pattern
import ml.classifiers.{Learner, NB}
import ml.models.Model
import util.{Datasets, Tempo}

import scala.util.Random

/**
 * Escolhe sempre a melhor query no momento,
 * i.e. aquela que vai aumentar a acurácia em todo o pool (acc medida roubando).
 * @param learner
 * @param pool
 * @param sampleSize
 * @param debug
 * @param testSet
 */
case class Optimal(learner: Learner, pool: Seq[Pattern], sampleSize: Int, debug: Boolean = false, testSet: Array[Pattern] = null)
  extends StrategyWithLearner {
  override val toString = "Optimal (accuracy) s" + sampleSize
  val abr = "Opt"
  lazy val rnd = new Random(0)
  var unlabeledSize = if (pool.length > 0) rest.length else -1
  //Strategy with empty pool exists only to provide its name.
  val id = -300

  def next(currentModel: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val (unlabeledSamp, unlabeledSampSize) = if (unlabeledSize > sampleSize) (rnd.shuffle(unlabeled).take(sampleSize), sampleSize) else (unlabeled, unlabeledSize)
    val (acc, p, m) = (unlabeledSamp map { pa =>
      val newModel = learner.update(currentModel)(pa)
      (newModel.accuracy(unlabeledSamp, unlabeledSampSize), pa, newModel)
    }).maxBy(_._1)
    if (testSet != null) println(m.accuracy(testSet))
    unlabeledSize -= 1
    p
  }
}

object FPRTest extends App {
  def learner = NB()

  //KNN(5, "eucl")
  //  val patts = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get).take(2000)
  val patts = new Random(0).shuffle(Datasets.arff("/home/davi/unversioned/experimentos/fourclusters.arff").right.get)
  val n = (patts.length * 0.5).toInt
  val s = Optimal(learner, patts.take(n), 500, debug = false, patts.drop(n).toArray)
  Tempo.start
  val b = s.queries.toList
  Tempo.print_stop
  //  val m = NB().build(b)
  //  println("---------------")
  //  println(m.accuracy(patts.drop(n)))
  //  println(m.accuracy(patts.take(n)))

  //  println(b.map(_.id))
  //  println(NB().build(b).accuracy(patts.drop(n)))
}