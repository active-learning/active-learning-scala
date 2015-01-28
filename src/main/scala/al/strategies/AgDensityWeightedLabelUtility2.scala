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

case class AgDensityWeightedLabelUtility2(pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends AgStrategyWithMapsLU with MarginMeasure {
   override val toString = "Density Weighted AgLU2 a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "ALU" + distance_name.take(3)
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 361 + (100 * (1 - alpha)).toInt
      case "cheb" => 381 + (100 * (1 - alpha)).toInt
      case "maha" => 391 + (100 * (1 - alpha)).toInt
      case "manh" => 371 + (100 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para DWAgLU2.")

   protected def next(mapU: => Map[Pattern, Double], mapsL: => Seq[Map[Pattern, Double]], unlabeled: Seq[Pattern], labeled: Seq[Pattern], hist: Seq[Int]) = {
      val selected = unlabeled maxBy { x =>
         val similarityU = mapU(x) / mapU.size.toDouble
         val similaritiesL = simL(mapsL, x, hist)
         math.pow(similarityU, beta) / math.pow(similaritiesL, alpha)
      }
      selected
   }

   def simL(mapsL: => Seq[Map[Pattern, Double]], patt: Pattern, hist: Seq[Int]) = {
      val tot = hist.sum
      mapsL.zipWithIndex.map { case (m, lab) =>
         val n = hist(lab).toDouble
         val p = n / tot
         math.pow(m(patt) / n, p)
      }.product
   }
}
