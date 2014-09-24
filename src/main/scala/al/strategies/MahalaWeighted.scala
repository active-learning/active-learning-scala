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

import ml.classifiers._
import ml.Pattern
import ml.Pattern
import org.math.array.{StatisticSample, LinearAlgebra}
import no.uib.cipr.matrix.{MatrixSingularException, DenseMatrix}
import ml.models.Model
import util.{ALDatasets, Datasets}

import scala.util.Random

case class MahalaWeighted(learner: Learner, pool: Seq[Pattern], beta: Double, debug: Boolean = false)
  extends StrategyWithLearner with StrategyWithMahala with MarginMeasure {
  override val toString = "Mahala Weighted b" + beta
  val abr = "DWM"
  val id = -100

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern = {
    try {
      val ud = maha_at_pool_for_mean(unlabeled)
      unlabeled maxBy {
        x =>
          val similarity = 1d / (1 + ud(x)) //mean includes x, but no problem since the pool is big
          (1 - margin(current_model)(x)) * math.pow(similarity, beta)
      }
    } catch {
      case ex: MatrixSingularException => error(s" MahalaW: singular matrix in ${pool.head.dataset().relationName()}! Defaulting to Random Sampling..."); unlabeled.head
    }
  }
}
