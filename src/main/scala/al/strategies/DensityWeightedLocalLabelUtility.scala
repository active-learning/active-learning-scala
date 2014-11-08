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
import ml.classifiers.Learner
import ml.models.Model

import scala.collection.immutable.ListMap

case class DensityWeightedLocalLabelUtility(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
  extends StrategyWithLearnerAndMapsLoLaU with MarginMeasure {
  override val toString = "Density Weighted LoLaU a" + alpha + " b" + beta + " (" + distance_name + ")"
  val abr = "DWLoLaU" + distance_name.take(3)
  val id = if (alpha == 1 && beta == 1) distance_name match {
    case "eucl" => 56
    case "cheb" => 58
    case "maha" => 59
    case "manh" => 57
  } else throw new Error("Parametros inesperados para DWLoLaU.")
  lazy val poolSize = pool.size

  protected def next(mapU: => Map[Pattern, ListMap[Pattern, Double]], mapsL: => Seq[Map[Pattern, ListMap[Pattern, Double]]], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern], hist: Seq[Int]) = {
    val labSize = labeled.size
    val toTakeL = math.min(labSize, math.max(5, labSize / 5))
    val toTakeU = math.min(100, math.max(1, (poolSize - labSize) / 5))
    val selected = unlabeled maxBy { x =>
      val similarityU = avgOfTop(mapU(x), toTakeU)
      val similaritiesL = simL(mapsL, x, toTakeL, hist)
      (1 - margin(current_model)(x)) * math.pow(similarityU, beta) / math.pow(similaritiesL, alpha)
    }
    selected
  }

  def avgOfTop(map: ListMap[Pattern, Double], toTake: Int) = {
    val seq = map.toIndexedSeq
    var similarity = 0d
    var i = 0
    while (i < toTake) {
      similarity += seq(i)._2
      i += 1
    }
    similarity /= toTake.toDouble
    similarity
  }

  def simL(mapsL: => Seq[Map[Pattern, ListMap[Pattern, Double]]], patt: Pattern, toTakeL: Int, hist: Seq[Int]) = {
    val tot = hist.size
    mapsL.map { m =>
      val n = hist(m.head._2.head._1.label.toInt).toDouble
      val p = n / tot
      math.pow(avgOfTop(m(patt), toTakeL), p)
    }.product
  }
}