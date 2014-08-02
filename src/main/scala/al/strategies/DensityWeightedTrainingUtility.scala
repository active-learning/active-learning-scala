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

case class DensityWeightedTrainingUtility(learner: Learner, pool: Seq[Pattern], alpha: Double, beta: Double, distance_name: String, debug: Boolean = false)
  extends StrategyWithLearnerAndMaps with MarginMeasure {
  override val toString = "Density Weighted TU a" + alpha + " b" + beta + " (" + distance_name + ")"

  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val selected = unlabeled maxBy {
      x =>
        val similarityU = mapU(x) / mapU.size.toDouble
        val similarityL = mapL(x) / mapL.size.toDouble
        (1 - margin(current_model)(x)) * math.pow(similarityU, beta) / math.pow(similarityL, alpha)
    }
    selected
  }
}

object DWTUTest extends App {
  val patts = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("abalone-11class").right.get.value).take(2000)
  val n = (patts.length * 0.5).toInt
  val s = DensityWeightedTrainingUtility(learner, patts.take(n), 1, 1, "eucl")
  val l = s.queries.toList
  val b = l.drop(11) foreach {
    q => m = learner.update(m)(q)
      println(m.accuracy(patts.drop(n)))
  }
  var m = learner.build(l.take(11))

  def learner = NB()
}