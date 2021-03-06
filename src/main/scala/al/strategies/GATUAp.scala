///*
// active-learning-scala: Active Learning library for Scala
// Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package al.strategies
//
//import ml.Pattern
//import ml.classifiers._
//import ml.models.Model
//import org.apache.commons.math3.stat.correlation.PearsonsCorrelation
//
//case class GATUAp(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
//   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
//   override val toString = "GATUAp a" + alpha + " b" + beta + " (" + distance_name + ")"
//   val abr = "\\textbf{GATUAp" + distance_name.take(3) + "}"
//   //+ beta
//   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
//      case "eucl" => 514326 + (100000 * (1 - alpha)).toInt
//      case "cheb" => 514328 + (100000 * (1 - alpha)).toInt
//      case "maha" => 514329 + (100000 * (1 - alpha)).toInt
//      case "manh" => 514327 + (100000 * (1 - alpha)).toInt
//   } else throw new Error("Parametros inesperados para GATUAp.")
//
//   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
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
//      var c = 0
//      entropias.find { e =>
//         c += 1
//         val res = e < olde
//         olde = e
//         res
//      }.getOrElse(agnostico = true)
//      if (n == 190) println(s"${labeled.head.dataset().relationName()} ${pool.size} $nclasses $natts: $c entropias calculadas")
//
//      val selected = unlabeled maxBy { x =>
//         val similarityU = mapU(x) / mapU.size.toDouble
//         val similarityL = mapL(x) / mapL.size.toDouble
//         if (agnostico)
//            math.pow(similarityU, beta) / math.pow(similarityL, alpha)
//         else
//            (1 - margin(current_model)(x)) * math.pow(similarityU, beta) / math.pow(similarityL, alpha)
//      }
//      selected
//   }
//}