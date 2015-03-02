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

import clean.lib.CM
import ml.Pattern
import ml.classifiers.KNNBatcha

case class GnoKNN(pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends AgStrategyMaps with MarginMeasure with EntropyMeasure with CM {
   override val toString = "GnoKNNa a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GnoKNNa" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 7599646 + (100000 * (1 - alpha)).toInt
      case "cheb" => 7599648 + (100000 * (1 - alpha)).toInt
      case "maha" => 7599649 + (100000 * (1 - alpha)).toInt
      case "manh" => 7599647 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GnoKNNa.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val model = KNNBatcha(5, "eucl", pool, weighted = true).build(labeled)
      val selected = unlabeled maxBy { x =>
         val similarityU = mapU(x) / mapU.size.toDouble
         val similarityL = mapL(x) / mapL.size.toDouble
         val dens = math.pow(similarityU, beta) / math.pow(similarityL, alpha)
         (1 - margin(model)(x)) * dens
      }
      selected
   }
}