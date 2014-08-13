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

case class SGmulti(learner: Learner, pool: Seq[Pattern], agreement: String, debug: Boolean = false)
  extends StrategySGmulti {
  override val toString = "SGmulti (" + agreement + ")"

  def controversial(unlabeled: Seq[Pattern], current_models: Array[Model]) =
    agreement match {
      case "consensus" => unlabeled find {
        pa =>
          val preds = current_models.map(mo => mo.predict(pa))
          preds.exists(_ != preds.head)
      } match {
        case Some(pattern) => pattern
        case None => unlabeled.head
      }
      case "majority" => unlabeled minBy {
        pa =>
          val preds = current_models.map(mo => mo.predict(pa))
          most_votes(preds)
      }
    }

  def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    if (selected != null) {
      for ((plot, i) <- plots.zipWithIndex) {
        plot.zera()
        for (p <- distinct_pool) plot.bola(p.x, p.y, models_to_visualize(i).predict(p), 9)
        for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
        plot.bola(selected.x, selected.y, -1, 15)
        plot.mostra()
      }
      plot.bola(selected.x, selected.y, -1, 15)
      plot.mostra()
    } else {
      plot.zera()
      if (models_to_visualize.head != null) for (p <- distinct_pool) plot.bola(p.x, p.y, if (models_to_visualize.map(mo => mo.predict(p)).exists(_ != models_to_visualize(0).predict(p))) 15 else models_to_visualize(0).predict(p), 9)
      for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
      plot.mostra()
    }
    Thread.sleep((delay * 1000).round.toInt)
  }

}

object SGTest extends App {
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

          val n = 11
          val s = SGmulti(KNNBatch(5, "eucl", pool), pool, "majority")
          println(s.queries.take(n + 5).toList.map(_.id))

          val s2 = SGmulti(KNNBatch(5, "eucl", pool), pool, "majority")
          val qs = s2.queries.take(n).toList
          println((qs ++ s2.resume_queries(qs).take(5).toList).map(_.id))

          if (s.queries.take(n + 7).toList.map(_.id) != (qs ++ s2.resume_queries(qs).take(7).toList).map(_.id)) println("problems")
          println("")
        }
      }
  }
}