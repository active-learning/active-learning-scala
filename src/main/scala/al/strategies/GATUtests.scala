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

import clean.lib.{AppWithUsage, FilterTrait, Ds, CM}
import ml.Pattern
import ml.classifiers._
import ml.models.Model
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.Tempo

import scala.util.Random

case class GATUtests(q: Int, learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
   override val toString = "GATU a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GATU" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 4123326 + (100000 * (1 - alpha)).toInt
      case "cheb" => 4123328 + (100000 * (1 - alpha)).toInt
      case "maha" => 4123329 + (100000 * (1 - alpha)).toInt
      case "manh" => 4123327 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GATU.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val ls = labeled.size
      val us = unlabeled.size
      //      val hist = Array.fill(nclasses)(0d)
      //      labeled foreach { lab =>
      //         val cla = lab.label.toInt
      //         hist(cla) += 1
      //      }
      //      //      val des = desb(hist)
      //      val s = hist.sum
      //      val des = normalized_entropy(hist.map(_ / s))
      //
      //      val rnd = new Random(seed)
      //      1 to labeled.size foreach (_ => rnd.nextDouble())
      //      val sampled = rnd.shuffle(unlabeled).take(10000)
      //
      //      var (imax, dmax, idmax, umax, lmax, umin, lmin) = (-1d, -1d, -1d, -1d, -1d, 1d, 1d)
      //      var ximax: Pattern = null
      //      var xdmax: Pattern = null
      //      var xumax: Pattern = null
      //      var xlmax: Pattern = null
      //      var xidmax: Pattern = null
      //      val list_i_id = sampled map { x =>
      //         val similarityU = mapU(x) / ss
      //         val similarityL = mapL(x) / ls
      //         val i = 1 - margin(current_model)(x)
      //         val u = math.pow(similarityU, beta)
      //         val l = math.pow(similarityL, alpha)
      //         val d = u / l
      //         val id = i * d
      //
      //         if (i > imax) {
      //            imax = i
      //            ximax = x
      //         }
      //         if (d > dmax) {
      //            dmax = d
      //            xdmax = x
      //         }
      //         if (u > umax) {
      //            umax = u
      //            xumax = x
      //         }
      //         if (l > lmax) {
      //            lmax = l
      //            xlmax = x
      //         }
      //         if (u < umin) {
      //            umin = u
      //            //            xumin = x
      //         }
      //         if (l < lmin) {
      //            lmin = l
      //            //            xlmin = x
      //         }
      //         if (id > idmax) {
      //            idmax = id
      //            xidmax = x
      //         }
      //         u -> l
      //      }
      //      val (a, b) = list_i_id.unzip
      //      val spear = new SpearmansCorrelation().correlation(a.toArray, b.toArray)
      //
      //      //            val s_i_id = 1d / (1 + d(ximax, xidmax))
      //      //            val s_d_id = 1d / (1 + d(xdmax, xidmax))
      //      //      val densObsoleta = spear > 0.99
      //      //      val densObsoleta = ximax == xidmax
      //      //      val densObsoleta = s_i_id < s_d_id
      //      //      print(s"${densObsoleta.compareTo(false)}")
      //      //      print(s"$spear")
      //      print(s"$lmin $lmax $umin ${10000 * umax} $des")

      unlabeled maxBy { x =>
         val similarityU = mapU(x) / us
         val similarityL = mapL(x) / ls
         val i = 1 - margin(current_model)(x)
         val u = math.pow(similarityU, beta)
         val l = math.pow(similarityL, alpha)
         val d = u / l
         if (ls >= q) i * d else d
      }

      //      val sim = 1d / (1 + d(xumax, xlmax))
      //      print(s"$sim")
   }
}


