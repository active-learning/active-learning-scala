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

import clean.lib.{FilterTrait, Ds, CM}
import ml.Pattern
import ml.classifiers._
import ml.models.Model
import util.Tempo

import scala.util.Random

case class GATU(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
   override val toString = "GATU a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GATU" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 524326 + (100000 * (1 - alpha)).toInt
      case "cheb" => 524328 + (100000 * (1 - alpha)).toInt
      case "maha" => 524329 + (100000 * (1 - alpha)).toInt
      case "manh" => 524327 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GATU.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      //      val hist = Array.fill(nclasses)(0d)
      //      val n = labeled.size
      //      var lastMin = 0
      //      val entropias = labeled.drop(nclasses).zipWithIndex.flatMap { case (lab, idx) =>
      //         val cla = lab.label.toInt
      //         hist(cla) += 1
      //         val s = idx + 1
      //         val histMin = hist.min.toInt
      //         if (hist.min > lastMin || s == n) {
      //            lastMin = histMin
      //            Some(normalized_entropy(hist.map(_ / s)))
      //         } else None
      //      }
      //      var agnostico = false
      //      var olde = entropias.headOption.getOrElse(-1d)
      //      entropias.find { e =>
      //         val res = e < olde
      //         olde = e
      //         res
      //      }.getOrElse(agnostico = true)

      val us = unlabeled.size
      val ls = labeled.size
      val vs = unlabeled map { x =>
         val similarityU = mapU(x) / us //mapU.size.toDouble
      val similarityL = mapL(x) / ls //mapL.size.toDouble
      //         if (agnostico)
      //            math.pow(similarityU, beta) / math.pow(similarityL, alpha)
      //         else
      val m = 1 - margin(current_model)(x)
         val u =math.pow(similarityU, beta)
         val l =math.pow(similarityL, alpha)
         val s = u / l
         (m, x, s, similarityU, similarityL, m*s)
      }
      val (m, selected, s, _, _, d) = vs maxBy (_._3)
      val un = (vs map (_._4)).min
      val um = (vs map (_._4)).sum/us
      val ux = (vs map (_._4)).max
      val ln = (vs map (_._5)).min
      val lm = (vs map (_._5)).sum/us
      val lx = (vs map (_._5)).max
      val mx = (vs map (_._1)).max
      val mn = (vs map (_._1)).min
      val mm = (vs map (_._1)).sum / us
      val sm = (vs map (_._3)).sum / us
      val sn = (vs map (_._3)).min
      val sx = (vs map (_._3)).max
      val dm = (vs map (_._6)).sum / us
      val dn = (vs map (_._6)).min
      val dx = (vs map (_._6)).max
      val mc=vs.map(_._1).count(_ <0.9*mx)
      val sc=vs.map(_._3).count(_ <0.9*sx)
      val dc=vs.map(_._6).count(_ <0.9*dx)
//      print(s"${(mm-mn)/(mx-mn)} ${(sm-sn)/(sx-sn)}")
      print(s"${sn/sx} $s ${math.sqrt(math.pow(m,1-sn/sx)*math.pow(s,sn/sx))}")
      selected
   }
}

object GATUTest extends App with CM with FilterTrait {
   val context = "GATUTest"
//      val ds = Ds("banana", readOnly = true)
      val ds = Ds("volcanoes-b2", readOnly = true)
//         val ds = Ds("abalone-3class", readOnly = true)
//   val ds = Ds("leaf", readOnly = true)
   val patts = new Random(2985).shuffle(ds.patterns).take(2000)
   println(patts.size)
   val (tr0, ts0) = patts.splitAt(2 * patts.size / 3)
   val (tr, binaf, zscof) = criaFiltro(tr0, 1)
   val ts = aplicaFiltro(ts0, 1, binaf, zscof)
   val l = RF()
   val s = GATU(l, tr, "eucl")
   var m = l.build(s.queries.take(tr.head.nclasses))
   val qs = s.queries.take(1000).drop(tr.head.nclasses).foreach { q =>
      m = l.update(m)(q)
      println(" " + "%7.5f".format(accBal(m.confusion(ts))))
   }
}
