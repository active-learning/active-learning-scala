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

import java.util.Calendar

import ml.classifiers.{KNN, Learner}
import ml.Pattern
import ml.models.Model
import util.{Lazy, Datasets}

import scala.util.Random

case class DensityWeighted(learner: Learner, pool: Seq[Pattern], beta: Double, distance_name: String, debug: Boolean = false)
  extends StrategyWithLearnerAndMaps with MarginMeasure {
  override val toString = "Density Weighted b" + beta + " (" + distance_name + ")"

  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val selected = unlabeled maxBy {
      x =>
        val similarity = mapU(x) / mapU.size.toDouble
        (1 - margin(current_model)(x)) * math.pow(similarity, beta)
    }
    selected
  }
}

object DWTest extends App {
  lazy val source = Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci") _
  source("iris") match {
    case Right(patts) =>
      0 until 5 foreach { run =>
        Datasets.kfoldCV(new Random(run).shuffle(patts), 5, false) { case (tr0, ts0, fold, minSize) =>

          //z-score
          lazy val f = Datasets.zscoreFilter(tr0)
          lazy val pool = {
            val tr = Datasets.applyFilterChangingOrder(tr0, f)
            val res = new Random(run * 100 + fold).shuffle(tr)
            res
          }
          lazy val testSet = {
            val ts = Datasets.applyFilterChangingOrder(ts0, f)
            new Random(run * 100 + fold).shuffle(ts)
          }

          if (run == 4 && fold == 4) {
            val n = 10
            val s = DensityWeighted(KNN(5, "eucl", pool), pool, 1, "eucl")
            s.queries.take(n + 1).toList
            //                        println((s.queries.take(n) ++ s.resume_queries(s.queries.take(n))).map(_.id).toList)
            println("")

            val s2 = DensityWeighted(KNN(5, "eucl", pool), pool, 1, "eucl")
            val qs = s2.queries.take(n).toList
            s.resume_queries(qs).take(1).toList

            //                        println(s2.queries.map(_.id).toList)

            //            val s = DensityWeighted(KNN(5, "eucl", pool), pool, 1, "eucl")
            //            val qs = s.queries.take(14)
            //            println(qs.map(_.id).toList)
            //            println(s.resume_queries(qs).map(_.id).toList)
            //
            //            val s2 = DensityWeighted(KNN(5, "eucl", pool), pool, 1, "eucl")
            //            val qs2 = s2.queries.take(15)
            //            println(qs2.map(_.id).toList)
            //            println(s2.resume_queries(qs2).map(_.id).toList)
          }
        }
      }
  }
}