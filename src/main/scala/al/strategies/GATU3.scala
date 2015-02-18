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
import util.XSRandom

case class GATU3(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
   override val toString = "GATU3 a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GATU3" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 533361 + (100000 * (1 - alpha)).toInt
      case "cheb" => 533381 + (100000 * (1 - alpha)).toInt
      case "maha" => 533391 + (100000 * (1 - alpha)).toInt
      case "manh" => 533371 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GATU3.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val hist = Array.fill(nclasses)(0d)
      labeled foreach { lab =>
         val cla = lab.label.toInt
         hist(cla) += 1
      }
      val d = desb(hist)

      val rnd = new XSRandom()
      rnd.setSeed(seed)
      1 to labeled.size foreach (_ => rnd.nextDouble())
      val p = rnd.nextDouble()

      val agnostico = p <= d

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

