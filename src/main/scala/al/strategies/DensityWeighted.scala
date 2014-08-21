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
import ml.classifiers._
import ml.models.Model
import util.Datasets

import scala.util.Random

case class DensityWeighted(learner: Learner, pool: Seq[Pattern], beta: Double, distance_name: String, debug: Boolean = false)
  extends StrategyWithLearnerAndMaps with MarginMeasure {
  override val toString = "Density Weighted b" + beta + " (" + distance_name + ")"
  val abr = "DW" + distance_name.take(3)

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

          def l = KNNBatch(5, "eucl", pool) // VFDT() //NB() //C45() //
        val n = 11
          val s = DensityWeighted(l, pool, 1, "eucl")
          println(s.queries.take(n + 5).toList.map(_.id))

          val s2 = DensityWeighted(l, pool, 1, "eucl")
          val qs = s2.queries.take(n).toList
          println(qs.map(_.id) + " " + (s2.resume_queries(qs).take(5).toList).map(_.id))
          if (s.queries.take(n + 7).toList.map(_.id) != (qs ++ s2.resume_queries(qs).take(7).toList).map(_.id)) println("problems")
          println("")
        }
      }
  }
}