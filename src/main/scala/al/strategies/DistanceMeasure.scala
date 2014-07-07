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
import ml.classifiers.Learner
import ml.neural.elm
import ml.neural.old.Neural
import no.uib.cipr.matrix.{MatrixSingularException, DenseMatrix, DenseVector}
import org.math.array.StatisticSample
import weka.core._

trait DistanceMeasure {
  val learner: Learner
  val pool: Seq[Pattern]
  val distance_name: String
  val debug: Boolean
  lazy val dataset = pool.head.dataset
  lazy val natts = dataset.numAttributes() - 1
  lazy val size = dataset.numInstances()
  lazy val d = distance_to(distance_name)

  lazy val instances_matrix = {
    val it = dataset.iterator()
    var list = List[Array[Double]]()
    while (it.hasNext) list = it.next.toDoubleArray.dropRight(1) :: list
    list.toArray
  }
  //TODO: CovarianceOps.invert() do EJML pode tirar vantagem na hora de inverter uma matrix de covariancia.
  //  Por outro lado, não se trata de um pinv()
  //  lazy val CovMatrixInv =     Neural.toArray(Neural.pinv(new DenseMatrix(StatisticSample.covariance(instances_matrix))))
  lazy val CovMatrixInv = Neural.pinv(new DenseMatrix(StatisticSample.covariance(instances_matrix)))
  lazy val euclidean_ruler = new EuclideanDistance(dataset)
  lazy val minkowski_ruler = new MinkowskiDistance(dataset)
  lazy val manhattan_ruler = new ManhattanDistance(dataset)
  lazy val chebyshev = new ChebyshevDistance(dataset)

  /**
   * Global Mahalanobis distance of a point to another.
   * @return
   */
  def mahadist(pa: Pattern, pb: Pattern) = {
    //todo: testar
    val x = pa.vector
    val y = pb.vector
    val diff = new DenseMatrix(1, pa.nattributes)
    val difft = new DenseVector(pa.nattributes)
    var i = 0
    val xl = pa.nattributes
    while (i < xl) {
      val v = x(i) - y(i)
      diff.set(0, i, v)
      difft.set(i, v)
      i += 1
    }
    val result = new DenseMatrix(1, pa.nattributes)
    try {
      diff.mult(CovMatrixInv, result)
      val result2 = new DenseVector(1)
      result.mult(difft, result2)
      Math.sqrt(result2.get(0))
    } catch {
      case _: MatrixSingularException => println("Singular matrix on mahalanobis calculation! Falling back to euclidean...")
        euclidean_ruler.distance(pa, pb)
    }
  }


  /**
   * Like Weka distances,
   * "maha" distance considers all brother patterns to be part of the distribution.
   * It is unclear whether or not the weka distances accept instances outside
   * the initial Instances object passed to the ruler's constructor.
   * @param distance_name
   * @return
   */
  def distance_to(distance_name: String) =
    distance_name match {
      case "eucl" => (pa: Pattern, pb: Pattern) => euclidean_ruler.distance(pa, pb)
      case "manh" => (pa: Pattern, pb: Pattern) => manhattan_ruler.distance(pa, pb)
      case "cheb" => (pa: Pattern, pb: Pattern) => chebyshev.distance(pa, pb)
      case "maha" => (pa: Pattern, pb: Pattern) => mahadist(pa, pb)
      case "mink" => (pa: Pattern, pb: Pattern) => minkowski_ruler.distance(pa, pb)
    }
}
