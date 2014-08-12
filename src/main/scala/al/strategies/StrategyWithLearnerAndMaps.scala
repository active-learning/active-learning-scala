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

trait StrategyWithLearnerAndMaps extends Strategy with DistanceMeasure {
  val learner: Learner

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val initial_mapU = unlabeled.map(x => x -> unlabeled.diff(Seq(x)).map(u => 1d / (1 + d(x, u))).sum).toMap
    val initial_mapL = unlabeled.map(x => x -> labeled.map(l => 1d / (1 + d(x, l))).sum).toMap

    val current_model = learner.build(labeled)

    //    var current_model = learner.build(labeled.take(3))
    //    labeled.drop(3) foreach (x => current_model = learner.update(current_model)(x))

    queries_rec(initial_mapU, initial_mapL, current_model, unlabeled, labeled)
  }

  private def queries_rec(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      if (debug) visual_test(null, unlabeled, labeled)
      val selected = next(mapU, mapL, current_model, unlabeled, labeled)

      lazy val newU = (mapU - selected) transform { case (pa, si) => si - 1d / (1 + d(selected, pa))}
      lazy val newL = (mapL - selected) transform { case (pa, si) => si + 1d / (1 + d(selected, pa))}

      val new_model = learner.update(current_model, fast_mutable = true)(selected)
      if (debug) visual_test(selected, unlabeled, labeled)
      selected #:: queries_rec(newU, newL, new_model, unlabeled.diff(Seq(selected)), labeled :+ selected)
    }
  }

  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    val current_model = learner.build(labeled)
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, current_model.predict(p), 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
    plot.mostra()
    Thread.sleep((delay * 1000).round.toInt)
  }
}


////
//package al.strategies
//
//import ml.{Model, Pattern}
//import ml.classifiers.Learner
//
//trait StrategyWithLearnerAndMaps extends Strategy with DistanceMeasure {
//  val learner: Learner
//
//  private def density(x: Pattern, population: Seq[Pattern], map: Map[Pattern, Double], dens: Double): (Double, Map[Pattern, Double]) = population match {
//    case Nil => (dens, map)
//    case he :: ta => val simi = map.getOrElse(he, 1d / (1 + d(x, he)))
//      density(x, ta, map + (he -> simi), dens + simi)
//  }
//
//  def density2mapdist(patterns1: Seq[Pattern], patterns2: Seq[Pattern]) = {
//    lazy val dens_dists = patterns1.map {
//      x => val (dens, inner_map) = density(x, patterns2.diff(Seq(x)), Map(), 0)
//        (x -> dens, x -> inner_map)
//    }
//    lazy val initial_dens = dens_dists.map(_._1).toMap
//    lazy val initial_dists = dens_dists.map(_._2)
//    (initial_dens, initial_dists)
//  }
//
//  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
//    //Calculate unlabeled and labeled densities for each unlabeled pattern.
//    lazy val (initial_mapU, initial_distsU) = density2mapdist(unlabeled, unlabeled)
//    lazy val (initial_mapL, initial_distsL) = density2mapdist(unlabeled, labeled)
//    lazy val current_model = learner.build(labeled)
//    val initial_dists_memo = (initial_distsU ++ initial_distsL).groupBy(_._1).map(x => x._1 -> x._2.map(_._2).reduce(_++_)) //merge maps
//    queries_rec(initial_mapU, initial_mapL, initial_dists_memo, current_model, unlabeled, labeled)
//  }
//
//  private def queries_rec(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], dists_memo: Map[Pattern, Map[Pattern, Double]], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
//    if (unlabeled.isEmpty) Stream.Empty
//    else {
//      if (debug) visual_test(null, unlabeled, labeled)
//      val selected = next(mapU, mapL, current_model, unlabeled, labeled)
//      lazy val newU = (mapU - selected) transform {
//        case (pa, si) =>
//          val dd = dists_memo.getOrElse(selected, throw new Error("Unexpected pattern: " + selected + "!")).getOrElse(pa, throw new Error("Unexpected pattern: " + pa + "!"))
//          si - dd
//      }
//      lazy val newL = (mapL - selected) transform {
//        case (pa, si) =>
//          val dd = dists_memo.getOrElse(selected, throw new Error("Unexpected pattern: " + selected + "!")).getOrElse(pa, throw new Error("Unexpected pattern: " + pa + "!"))
//          si + dd
//      }
//
//      val new_model = current_model.update(selected)
//      if (debug) visual_test(selected, unlabeled, labeled)
//      selected #:: queries_rec(newU, newL, dists_memo, new_model, unlabeled.diff(Seq(selected)), labeled :+ selected)
//    }
//  }
//
//  protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern
//
//  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
//    val current_model = learner.build(labeled)
//    plot.zera()
//    for (p <- distinct_pool) plot.bola(p.x, p.y, current_model.predict(p), 9)
//    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
//    if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
//    plot.mostra()
//    Thread.sleep((delay * 1000).round.toInt)
//  }
//}
