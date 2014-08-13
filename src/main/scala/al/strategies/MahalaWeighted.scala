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

import ml.classifiers.{KNN, VFDT, Learner}
import ml.Pattern
import ml.Pattern
import org.math.array.{StatisticSample, LinearAlgebra}
import no.uib.cipr.matrix.{MatrixSingularException, DenseMatrix}
import ml.models.Model
import util.Datasets

import scala.util.Random

case class MahalaWeighted(learner: Learner, pool: Seq[Pattern], beta: Double, debug: Boolean = false)
  extends StrategyWithLearner with StrategyWithMahala with MarginMeasure {
  override val toString = "Mahala Weighted b" + beta

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern = {
    try {
      val ud = maha_at_pool_for_mean(unlabeled)
      unlabeled maxBy {
        x =>
          val similarity = 1d / (1 + ud(x)) //mean includes x, but no problem since the pool is big
          (1 - margin(current_model)(x)) * math.pow(similarity, beta)
      }
    } catch {
      case ex: MatrixSingularException => println(" MahalaW: singular matrix! Defaulting to Random Sampling..."); unlabeled.head
    }
  }
}

object MWTest extends App {
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
            val s = MahalaWeighted(KNN(5, "eucl", pool), pool, 1)
            println(s.queries.take(n + 7).toList.map(_.id))

            val s2 = MahalaWeighted(KNN(5, "eucl", pool), pool, 1)
            val qs = s2.queries.take(n).toList
            println((qs ++ s.resume_queries(qs).take(7).toList).map(_.id))
          }
        }
      }
  }
}