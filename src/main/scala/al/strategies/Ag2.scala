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
import ml.classifiers._
import ml.models.Model

case class Ag2(hlimit:Int, learner:Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure with CM {
   override val toString = "STU a" + alpha + " b" + beta + " (" + distance_name + ")"
   val maxFolds = 10
   val abr = "\\textbf{STU" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 599996 + (100000 * (1 - alpha)).toInt
      case "cheb" => 599998 + (100000 * (1 - alpha)).toInt
      case "maha" => 599999 + (100000 * (1 - alpha)).toInt
      case "manh" => 599997 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para STU.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], model:Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
//      val hist = Array.fill(nclasses)(0)
//      var histMin = 0
      val ls = labeled.size
//      labeled foreach { ex =>
//         val cla = ex.label.toInt
//         hist(cla) += 1
//         histMin = hist.min
//      }
//      lazy val model = if (histMin >= hlimit) {
//         val les = Seq(
//            (s: Int) => KNNBatcha(5, "eucl", pool, weighted = true) //2
//            , (s: Int) => C45() //3
//            , (s: Int) => RF(s, 5, 5)
//            , (s: Int) => NBBatch()
//            , (s: Int) => VFDT()
//         )
//
//         val accs = if (histMin < maxFolds / 2 && ls < 70) {
//            val larr = labeled.toArray
//            les.map { l =>
//               val loo = Datasets.LOO(larr) { (tr, ex) =>
//                  val m = l(seed * ex.id * larr.size).build(tr)
//                  m.hit(ex)
//               }
//               l -> loo.count(_ == true).toDouble
//            }
//         } else {
//            val lvec = labeled.toVector
//            les.map { l =>
//               val cv = Datasets.kfoldCV(lvec, math.min(histMin, maxFolds)) { (tr, ts, f, ms) =>
//                  val m = l(seed * ts.head.id * f).build(tr)
//                  m.accuracy(ts)
//               }
//               l -> cv.sum
//            }
//         }
//         val bestLearner = accs.maxBy(_._2)._1(seed * ls)
//         Some(bestLearner.build(labeled))
//      } else None

      val selected = unlabeled maxBy { x =>
         val similarityU = mapU(x) / mapU.size.toDouble
         val similarityL = mapL(x) / mapL.size.toDouble
         val dens = math.pow(similarityU, beta) / math.pow(similarityL, alpha)
         if (ls >= hlimit) (1 - margin(model)(x)) * dens else dens
      }
      selected
   }
}