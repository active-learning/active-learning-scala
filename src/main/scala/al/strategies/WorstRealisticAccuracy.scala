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
import util.Datasets

import scala.util.Random

case class WorstRealisticAccuracy(learner: Learner, pool: Seq[Pattern], testSet: Seq[Pattern], debug: Boolean = false)
  extends StrategyWithLearner {
  override val toString = "Perfect (accuracy)"

  def next(currentModel: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    ???
    val (acc, p, m) = (unlabeled map { pa =>
      val newModel = learner.update(currentModel)(pa)
      (newModel.accuracy(unlabeled), pa, newModel)
    }).minBy(_._1)

    println(m.accuracy(testSet))
    //    if (acc >= passiveAccuracy) stop = true
    p
  }
}

object WRTest extends App {
  def learner = NB()

  //KNN(5, "eucl")
  val patts = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get).take(2000)
  val n = (patts.length * 0.5).toInt
  val s = WorstRealisticAccuracy(learner, patts.take(n), patts.drop(n))
  val b = s.queries.toList
  //  val m = NB().build(b)
  //  println("---------------")
  //  println(m.accuracy(patts.drop(n)))
  //  println(m.accuracy(patts.take(n)))

  //  println(b.map(_.id))
  //  println(NB().build(b).accuracy(patts.drop(n)))
}