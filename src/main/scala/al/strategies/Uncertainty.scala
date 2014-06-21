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

case class Uncertainty(learner: Learner, pool: Seq[Pattern], debug: Boolean = false)
  extends StrategyWithLearner {
  override val toString = "Uncertainty"

  def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val selected = unlabeled minBy (pa => current_model.distribution(pa).max)
    selected
  }
}

object UTest extends App {
  def learner = NB()
//  val patts = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get).take(2000)
val patts = new Random(0).shuffle(Datasets.arff(true)("/home/davi/unversioned/experimentos/fourclusters.arff").right.get)
  val n = (patts.length * 0.5).toInt
  val s = Uncertainty(learner, patts.take(n))
  val l = s.queries.toList
  var m = learner.build(l.take(patts.head.nclasses))
  val b = l.drop(patts.head.nclasses) foreach {
    q => m = learner.update(m)(q)
      println(m.accuracy(patts.drop(n)))
  }
}