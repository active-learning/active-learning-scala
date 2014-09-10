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
import ml.classifiers.{KNNBatch, Learner}
import ml.models.Model
import no.uib.cipr.matrix.MatrixSingularException
import util.{ALDatasets, Datasets}

import scala.util.Random

case class MahalaWeightedTrainingUtility(learner: Learner, pool: Seq[Pattern], alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
  extends StrategyWithMahala with MarginMeasure {
  override val toString = "Mahala Weighted TU a" + alpha + " b" + beta
  val abr = "DWTUM"
  val id = if (alpha == 1 && beta == 1) 10 else throw new Error("Parametros inesperados para MAhalaTU.")

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern = {
    try {
      val ud = maha_at_pool_for_mean(unlabeled)
      val ld = maha_at_pool_for_mean(labeled)
      unlabeled maxBy {
        x =>
          val similarityU = 1d / (1 + ud(x)) //mean includes x, but no problem since the pool is big
        val similarityL = 1d / (1 + ld(x))
          (1 - margin(current_model)(x)) * math.pow(similarityU, alpha) / math.pow(similarityL, beta)
      }
    } catch {
      case ex: MatrixSingularException => println(" MahalaWTU: singular matrix! Defaulting to Random Sampling..."); unlabeled.head
    }
  }
}

object MWTUTest extends App {
  lazy val source = ALDatasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci") _
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
          val s = MahalaWeightedTrainingUtility(KNNBatch(5, "eucl", pool), pool, 1, 1)
          println(s.queries.take(n + 5).toList.map(_.id))

          val s2 = MahalaWeightedTrainingUtility(KNNBatch(5, "eucl", pool), pool, 1, 1)
          val qs = s2.queries.take(n).toList
          println((qs ++ s2.resume_queries(qs).take(5).toList).map(_.id))

          if (s.queries.take(n + 7).toList.map(_.id) != (qs ++ s2.resume_queries(qs).take(7).toList).map(_.id)) println("problems")
          println("")
        }
      }
  }
}