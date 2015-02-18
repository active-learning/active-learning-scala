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

case class GATU2(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
   override val toString = "GATU2 a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GATU2" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 56726 + (100000 * (1 - alpha)).toInt
      case "cheb" => 56728 + (100000 * (1 - alpha)).toInt
      case "maha" => 56729 + (100000 * (1 - alpha)).toInt
      case "manh" => 56727 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GATU2.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val hist = Array.fill(nclasses)(0d)
      val entropias = labeled map { lab =>
         val cla = lab.label.toInt
         hist(cla) += 1
         val s = hist.sum
         normalized_entropy(hist.map(_ / s))
      }
      var agnostico = false //começa com gnostico, pois na segunda vez sempre inverte
      var olde = entropias(nclasses - 1)
      entropias.drop(nclasses).foreach { e =>
         if (e < olde) agnostico = !agnostico
         olde = e
      }

      val selected = unlabeled maxBy { x =>
         val similarityU = mapU(x) / mapU.size.toDouble
         val similarityL = mapL(x) / mapL.size.toDouble
         if (agnostico)
            math.pow(similarityU, beta) / math.pow(similarityL, alpha)
         else
            1 - margin(current_model)(x)
      }
      selected
   }
}

