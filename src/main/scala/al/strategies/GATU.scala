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
      val hist = Array.fill(nclasses)(0d)
      val n = labeled.size
      var lastMin = 0
      val entropias = labeled.drop(nclasses).zipWithIndex.flatMap { case (lab, idx) =>
         val cla = lab.label.toInt
         hist(cla) += 1
         val s = idx + 1
         val histMin = hist.min.toInt
         if (hist.min > lastMin || s == n) {
            lastMin = histMin
            Some(normalized_entropy(hist.map(_ / s)))
         } else None
      }
      var agnostico = false
      var olde = entropias.headOption.getOrElse(-1d)
      entropias.find { e =>
         val res = e < olde
         olde = e
         res
      }.getOrElse(agnostico = true)

      val selected = unlabeled maxBy { x =>
         val similarityU = mapU(x) / mapU.size.toDouble
         val similarityL = mapL(x) / mapL.size.toDouble
         if (agnostico)
            math.pow(similarityU, beta) / math.pow(similarityL, alpha)
         else
            (1 - margin(current_model)(x)) * math.pow(similarityU, beta) / math.pow(similarityL, alpha)
      }
      selected
   }
}

object GATUTest extends App with CM with FilterTrait {
   val context = "GATUTest"
   //   val ds = Ds("abalone-11class", readOnly = true)
   val ds = Ds("phoneme", readOnly = true)
   val patts = new Random(2985).shuffle(ds.patterns).take(1000)
   val (tr0, ts0) = patts.splitAt(patts.size / 2)
   val (tr, binaf, zscof) = criaFiltro(tr0, 1)
   val ts = aplicaFiltro(ts0, 1, binaf, zscof)
   //   val res = Seq(C45(), KNNBatch(5, "eucl", tr, true), NinteraELM(), CIELMBatch(), NBBatch(), RF()) map { l =>
   val res = Seq(CIELMBatch()).par map { l =>
      //   val res = Seq(NinteraELM()).par map { l =>
      val r = l.abr -> Tempo.timev {
         //         val s = ExpModelChange(l, tr)
         val s = RandomSampling(tr)
         //         val s = DensityWeightedTrainingUtility(l, tr, "eucl")
         val qs = s.queries.drop(tr.head.nclasses).take(100).toList
         "%5.3f".format(kappa(l.build(qs).confusion(ts)))
      }
      r._1 + "\t\t\t" + r._2
   }
   println(s"")
   res foreach println
}

// RF antigo, aba11, |tr|=100, take 200
//(5NN,        (0.198,6.559))
//(C4.5w,      (0.250,5.655))
//(CIELMBatch, (0.222,11.27))
//(ELM,        (0.218,7.385))
//(NBBatch,    (0.234,15.571))
//(RF,         (0.239,33.805))

// RF antigo, aba11, |tr| livre, take 50
//(CIELMBatch,(0.183,18.891))
//(5NN,(0.206,9.532))
//(ELM,(0.213,8.372))
//(C4.5w,(0.219,6.848))
//(RF,(0.225,40.101))
//(NBBatch,(0.233,27.509))
