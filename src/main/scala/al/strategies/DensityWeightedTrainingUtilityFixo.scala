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
import ml.classifiers.{RF, Learner}
import ml.models.Model

case class DensityWeightedTrainingUtilityFixo(poolForLearner: Seq[Pattern], learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false, print: Boolean = false)
  extends StrategyWithLearnerAndMaps with MarginMeasure {
  override lazy val toString = "Density Weighted TU" + learner.limpa + " a" + alpha + " b" + beta + " (" + distance_name + ")"
  lazy val abr = "TU" + distance_name.take(3)
  lazy val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
    case "eucl" => 127176 + convlid(learner.id)
    case "cheb" => 127178 + convlid(learner.id)
    case "maha" => 127179 + convlid(learner.id)
    case "manh" => 127177 + convlid(learner.id)
  } else throw new Error("Parametros inesperados para DWTURF.")

  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val us = unlabeled.size
    val ls = labeled.size
    if (!print) {
      unlabeled maxBy { x =>
        val similarityU = mapU(x) / us
        val similarityL = mapL(x) / ls
        val atu = math.pow(similarityU, beta) / math.pow(similarityL, alpha)
        val mar = 1 - margin(current_model)(m(x.id))
        val tu = mar * atu
        tu
      }
    } else {
      val ma = unlabeled map { x =>
        val similarityU = mapU(x) / us
        val similarityL = mapL(x) / ls
        val atu = math.pow(similarityU, beta) / math.pow(similarityL, alpha)
        val mar = 1 - margin(current_model)(m(x.id))
        val tu = mar * atu
        (x, atu, mar, tu)
      }
      val msorted = ma.sortBy(_._4)
      val (selected, atuse, marse, tuse) = msorted.last
      val atumx = msorted.map(_._2).max
      val marmx = msorted.map(_._3).max
      val tumx = msorted.map(_._4).max
      println(s"$ls $atumx $marmx $tumx")
      selected
    }
  }
}
