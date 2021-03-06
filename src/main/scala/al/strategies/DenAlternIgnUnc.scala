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

import scala.util.Random

case class DenAlternIgnUnc(learner: Learner, pool: Seq[Pattern], numNeigs: Int, poolForLearner: Seq[Pattern], distance_name: String = "eucl", debug: Boolean)
  extends StrategyWithLearnerAndMapsAndKnowledgeModel
    with MarginMeasure {
  val id: Int = 129587
  val abr: String = "ign"

  protected def next(explore: Boolean, predictions: Seq[Double], knowledgeModel: Model, mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled0: Seq[Pattern], labeled: Seq[Pattern]) = {
    val unlabeled = new Random(unlabeled0.size).shuffle(unlabeled0).take(n)
    val us = unlabeled.size
    val ls = labeled.size

    val densU = unlabeled map { x => x -> 0*mapU(x) / us }
    val densL = unlabeled map { x => x -> 0*mapL(x) / ls }
    lazy val mars = unlabeled map { x => x -> (1 - margin(current_model)(m(x.id))) }
    lazy val knos = unlabeled map { x => x -> knowledgeModel.distribution(m(x.id)).head }

    val knosden = rank(densU).zip(rank(knos)).map { case (a, b) => math.max(a,b) }
    val marsden = rank(densU).zip(rank(mars)).map { case (a, b) => math.max(a,b) }

    val (selected, _) = if (explore) {
      val rankTups = unlabeled.sortBy(_.id) zip (rank(densU), rank(densL), knosden).zipped.toList
      rankTups map worstPosition3 minBy (_._2)
    } else {
      val rankTups = unlabeled.sortBy(_.id) zip (rank(densU), rank(densL), marsden).zipped.toList
      rankTups map worstPosition3 minBy (_._2)
    }
    selected
  }

  def rank(patvals: Seq[(Pattern, Double)]) = patvals.sortBy(-_._2).zipWithIndex.sortBy(_._1._1.id) map (_._2)
}