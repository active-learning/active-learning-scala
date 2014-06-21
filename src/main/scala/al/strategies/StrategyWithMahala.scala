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
import org.math.array.{StatisticSample, LinearAlgebra}
import no.uib.cipr.matrix.DenseMatrix
import ml.neural.old.Neural

trait StrategyWithMahala extends StrategyWithLearner {
  lazy val maha_at_pool_for_mean = mahalanobis_to_mean(distinct_pool)

  private def premaha(patterns: Seq[Pattern]) = Neural.pinv(new DenseMatrix(StatisticSample.covariance((patterns map (_.array)).toArray)))

  /**
   * Mahalanobis distance of a point to the mean.
   */
  protected def mahalanobis_to_mean(pool_patterns: Seq[Pattern]) = {
    val Sinv = premaha(pool_patterns)
    (patterns_to_average: Seq[Pattern]) => {
      val patterns_matrix = (patterns_to_average map (_.array)).toArray
      val mean = StatisticSample.mean(patterns_matrix)
      (pa: Pattern) => mahalanobis0(mean, Sinv)(pa)
    }
  }

  /**
   * Mahalanobis distance of a point to Y.
   */
  protected def mahalanobis_to(pool_patterns: Seq[Pattern]) = {
    val Sinv = premaha(pool_patterns)
    (y: Pattern) => (pa: Pattern) => mahalanobis0(y.array, Sinv)(pa)
  }

  protected def mahalanobis0(mean: Array[Double], Sinv: DenseMatrix)(pa: Pattern): Double = {
    ??? //testar
    val x = pa.vector
    val diff = new DenseMatrix(1, pa.nattributes)
    var i = 0
    val xl = pa.nattributes
    while (i < xl) {
      diff.set(0, i, x(i) - mean(i))
      i += 1
    }
    val result = new DenseMatrix(1, pa.nattributes)
    diff.mult(Sinv, result)
    val result2 = new DenseMatrix(pa.nattributes, 1)
    result.mult(diff.transpose, result2)
    val d = Math.sqrt(result2.get(0, 0))
    //         println("d = " + d)
    d
  }
}

