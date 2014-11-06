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

trait StrategyWithLearnerAndMapsLoU extends Strategy with DistanceMeasure {
  val learner: Learner

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val initial_mapU = unlabeled.map { u1 =>
      u1 -> ListMap(unlabeled.diff(Seq(u1)).take(200).map { u2 =>
        u2 -> 1d / (1 + d(u1, u2))
      }.sortBy(_._2): _*)
    }.toMap

    val initial_mapL = unlabeled.map { u =>
      u -> ListMap(labeled.map { l =>
        l -> 1d / (1 + d(u, l))
      }.sortBy(_._2): _*)
    }.toMap

    val current_model = learner.build(labeled)
    queries_rec(initial_mapU, initial_mapL, current_model, unlabeled, labeled)
  }

  private def queries_rec(mapU: => Map[Pattern, ListMap[Pattern, Double]], mapL: => Map[Pattern, ListMap[Pattern, Double]], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      if (debug) visual_test(null, unlabeled, labeled)
      val selected = next(mapU, mapL, current_model, unlabeled, labeled)

      //apaga origem selected e passa destino selected de mapU para mapL.
      lazy val newU = (mapU - selected) transform { case (p, lm) => lm - selected}
      lazy val newL = (mapL - selected) transform { case (p, lm) =>
        val dist = 1d / (1 + d(p, selected))
        val (antes, depois) = lm.span(_._2 < dist)
        antes ++ ListMap(selected -> dist) ++ depois
      }

      val new_model = learner.update(current_model, fast_mutable = true)(selected)
      if (debug) visual_test(selected, unlabeled, labeled)
      selected #:: queries_rec(newU, newL, new_model, unlabeled.diff(Seq(selected)), labeled :+ selected)
    }
  }

  protected def next(mapU: Map[Pattern, ListMap[Pattern, Double]], mapL: Map[Pattern, ListMap[Pattern, Double]], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    val current_model = learner.build(labeled)
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, current_model.predict(p).toInt, 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
    plot.mostra()
    Thread.sleep((delay * 1000).round.toInt)
  }

  def sortedMerge(a: ListMap[(Pattern, Pattern), Double], b: ListMap[(Pattern, Pattern), Double]): ListMap[(Pattern, Pattern), Double] =
    if (a.isEmpty) b
    else if (a.head._2 <= b.head._2) ListMap(a.head) ++ sortedMerge(a.drop(1), b)
    else sortedMerge(b, a)
}