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
import util.Graphics.Plot

import scala.util.Random

case class IgnUncDen(learner: Learner, pool: Seq[Pattern], numNeigs: Int, poolForLearner: Seq[Pattern], distance_name: String = "eucl", debug: Boolean)
  extends StrategyWithLearnerAndMapsAndKnowledgeModel with MarginMeasure {
  val id: Int = 129587
  val abr: String = "ign"
  val plo = new Plot

  protected def next(explore: Boolean, predictions: Seq[Double], knowledgeModel: Model, mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled0: Seq[Pattern], labeled: Seq[Pattern]) = {
    val unlabeled = new Random(unlabeled0.size).shuffle(unlabeled0).take(n).sortBy(_.id)

    val densU = unlabeled map { x => x -> mapU(x) }
    val densL = unlabeled.zip(densU) map { case(x,(y,u)) => x -> u / mapL(x) }
    lazy val mars = unlabeled map { x => x -> (1 - margin(current_model)(m(x.id))) }
    lazy val knos = unlabeled map { x => x -> knowledgeModel.distribution(m(x.id)).head }

    if (labeled.size == 2) {
      plo.zera()
      val r = rank(densU)
      unlabeled.zipWithIndex.foreach { case (p, i) => plo.bola_intense(p.x, p.y, 1 - (r(i) - r.min).toFloat / (r.max - r.min).toFloat, 9) }
    }
    plo.mostra()

    val rankTups = unlabeled zip zipa(rank(densL), rank(densL), rank(mars), rank(knos))
    val (selected, _) = rankTups map worstPosition4 minBy (_._2)

    //    val (selected, _) = if (explore) {
    //      println("explorando")
    //    } else {
    //      println("prospectando")
    //    }
    selected
  }

  def zipa(a: Seq[Int], b: Seq[Int], c: Seq[Int], d: Seq[Int]) =
    a.zip(b).zip(c.zip(d)) map { case ((a, b), (c, d)) => (a, b, c, d) }

  def rank(patvals: Seq[(Pattern, Double)]) = patvals.sortBy(-_._2).zipWithIndex.sortBy(_._1._1.id) map (_._2)
}