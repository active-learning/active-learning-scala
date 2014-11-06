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
import ml.models.Model

import scala.collection.immutable.ListMap

case class DensityWeightedLocalUtility(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
  extends StrategyWithLearnerAndMapsLoU with MarginMeasure {
  override val toString = "Density Weighted LoU a" + alpha + " b" + beta + " (" + distance_name + ")"
  val abr = "DWLoU" + distance_name.take(3)
  val id = if (alpha == 1 && beta == 1) distance_name match {
    case "eucl" => 46
    case "cheb" => 48
    case "maha" => 49
    case "manh" => 47
  } else throw new Error("Parametros inesperados para DWLoU.")
  lazy val poolSize = pool.size

  protected def next(mapU: Map[Pattern, ListMap[Pattern, Double]], mapL: Map[Pattern, ListMap[Pattern, Double]], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val labSize = labeled.size
    val toTakeL = math.min(labeled.size, math.max(5, labSize / 5))
    val toTakeU = math.min(100, math.max(5, (poolSize - labSize) / 5))
    val selected = unlabeled maxBy { x =>
      val ui = mapU(x).toIndexedSeq
      val li = mapL(x).toIndexedSeq

      var similarityU = 0d
      var i = 0
      while (i < toTakeU) {
        similarityU += ui(i)._2
        i += 1
      }
      similarityU /= toTakeU.toDouble

      var similarityL = 0d
      i = 0
      while (i < toTakeL) {
        similarityL += li(i)._2
        i += 1
      }
      similarityL /= toTakeL.toDouble

      (1 - margin(current_model)(x)) * similarityU / similarityL
      //      (1 - margin(current_model)(x)) * math.pow(similarityU, beta) / math.pow(similarityL, alpha)
    }
    selected
  }

  /*
    SOverflow site
   */
  def top[T](n: Int, iter: Iterable[T])(implicit ord: Ordering[T]): Iterable[T] = {
    def partitionMax(acc: Iterable[T], it: Iterable[T]): Iterable[T] = {
      val max = it.max(ord)
      val (nextElems, rest) = it.partition(ord.gteq(_, max))
      val maxElems = acc ++ nextElems
      if (maxElems.size >= n || rest.isEmpty) maxElems.take(n)
      else partitionMax(maxElems, rest)
    }
    if (iter.isEmpty) iter.take(0)
    else partitionMax(iter.take(0), iter)
  }
}
