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

import java.util.Calendar

import ml.classifiers.{KNN, Learner}
import ml.Pattern
import ml.models.Model
import util.{Lazy, Datasets}

import scala.util.Random

case class DensityWeighted(learner: Learner, pool: Seq[Pattern], beta: Double, distance_name: String, debug: Boolean = false)
  extends StrategyWithLearnerAndMaps with MarginMeasure {
  override val toString = "Density Weighted b" + beta + " (" + distance_name + ")"

  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val selected = unlabeled maxBy {
      x =>
        val similarity = mapU(x) / mapU.size.toDouble
        (1 - margin(current_model)(x)) * math.pow(similarity, beta)
    }
    selected
  }
}
