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

import ml.classifiers.Learner
import ml.Pattern
import ml.models.Model
import scala.util.Random

/**
 * Maybe it is possible to implement an incremental Mahalanobis distance calculation.
 * Takes into account only U and L sets at the moment.
 *
 * @param learner
 * @param pool
 * @param beta
 * @param sample subsample to reduce density calculations, all the pool is still searched for candidates
 * @param debug
 */
case class MahalaWeightedRefreshed(learner: Learner, pool: Seq[Pattern], beta: Double, sample: Int, debug: Boolean = false)
  extends StrategyWithMahala with MarginMeasure with Sample {
  override val toString = "Mahala Weighted Refreshed b" + beta + " s" + sample
  val abr = "DWMR"
  val id = -3101

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern = {
    val unlabeledSize = unlabeled.size
    val rnd = new Random(unlabeledSize)
    val (unlabeledSamp, unlabeledSampSize) = if (unlabeledSize > sample_internal) (rnd.shuffle(unlabeled).take(sample_internal), sample_internal) else (unlabeled, unlabeledSize)
    //    val res = try {
    val ud = mahalanobis_to(unlabeled)
    unlabeled maxBy {
      x =>
        val similarityU = (unlabeledSamp.diff(Seq(x)) map (u => 1d / (1 + ud(x)(u)))).sum / unlabeledSampSize.toDouble
        (1 - margin(current_model)(x)) * math.pow(similarityU, beta)
    }
    //    } catch {
    //      case ex: MatrixSingularException => error(s" MahalaW: singular matrix in ${pool.head.dataset().relationName()}! Defaulting to Random Sampling..."); unlabeled.head
    //    }
    //    res
  }
}
