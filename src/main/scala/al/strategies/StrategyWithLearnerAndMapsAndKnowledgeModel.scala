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

trait StrategyWithLearnerAndMapsAndKnowledgeModel extends Strategy with DistanceMeasure {
  val numNeigs: Int
  val learner: Learner
  val poolForLearner: Seq[Pattern]
  var knoSet = Seq[Pattern]()
  lazy val m = poolForLearner.map(x => x.id -> x).toMap
  lazy val w = 1d / pool.size
  lazy val plots = Array.fill(nclasses)(new Plot)
  var knowledgeModelForVisu: Model = null

  def worstPosition3(tup: (Pattern, (Int, Int, Int))) = tup match {
    case (e, (a, b, c)) => e -> math.max(math.max(a, b), c)
  }

  def worstPosition4(tup: (Pattern, (Int, Int, Int, Int))) = tup match {
    case (e, (a, b, c, d)) => e -> math.max(math.max(a, b), math.max(b, c))
  }

  def den(set: Seq[Pattern], signal:Int) = {
    pool.map { x =>
      //      val allbut = (unlabeled ++ labeled).diff(Seq(x))
      val dists = set.map { s =>
        //      val dists = allbut.map { u =>
        s -> signal * 1d / (1 + d(x, s))
      }
      val neigs = dists.sortBy(_._2).take(numNeigs).map(_._2)
      x -> neigs.sum
    }.toMap
  }

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val initial_mapU = den(unlabeled, 1)
    val initial_mapL = den(labeled, -1)
    val decisionModel = learner.build(labeled.map(x => m(x.id)))
    val class0set = unlabeled.map(_.relabeled_reweighted(0, w, new_missed = false))
    val class1set = labeled.map(_.relabeled_reweighted(1, 1, new_missed = false))
    val knowledgeSet = class0set ++ class1set
    val knowledgeModel = learner.build(knowledgeSet)
    val predictions = pool map decisionModel.predict
    queries_rec(explore = true, predictions, knowledgeModel, knowledgeSet, initial_mapU, initial_mapL, decisionModel, unlabeled, labeled)
  }

  private def queries_rec(explore: Boolean, predictions: Seq[Double], knowledgeModel: Model, knowledgeSet: Seq[Pattern], mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      //      if (debug) visual_test(null, unlabeled, labeled)
      val selected = next(explore, predictions, knowledgeModel, mapU, mapL, current_model, unlabeled, labeled)
      val newLabeled = labeled :+ selected
      val newUnlabeled = unlabeled.diff(Seq(selected))
      //agora refaço a densidade, pois ajuda nas consultas eliminar os rotulados .  OLD: ignorance não requer que densidade seja alterada. está usando rotulados + unlabeled
      val newU = den(newUnlabeled, 1) //(mapU - selected) transform { case (pa, si) => si - 1d / (1 + d(selected, pa)) }
      val newL = den(newLabeled, -1)

      val new_knowledgeSet = knowledgeSet.updated(knowledgeSet.indexOf(selected), selected.relabeled_reweighted(1, 1, new_missed = false))
      val new_decisionModel = learner.update(current_model, fast_mutable = true)(m(selected.id))
      val new_knowledgeModel = learner.build(new_knowledgeSet)

      val newPredictions = pool map new_decisionModel.predict
      val newExplore = if (this.isInstanceOf[DenAlternIgnUnc] && predictions == newPredictions) {
        if (explore) println("Prospecting...") else println("Exploring...")
        !explore
      } else explore

      if (debug) {
        knowledgeModelForVisu = knowledgeModel
        knoSet = knowledgeSet
        visual_test(selected, unlabeled, newLabeled)
      }

      selected #:: queries_rec(newExplore, newPredictions, new_knowledgeModel, new_knowledgeSet, newU, newL, new_decisionModel, newUnlabeled, newLabeled)
    }
  }

  protected def next(explore: Boolean, predictions: Seq[Double], knowledgeModel: Model, mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    val current_model = learner.build(labeled)
    if (knowledgeModelForVisu != null)
      for (((plot, m), unl) <- plots.zip(Seq(knowledgeModelForVisu, current_model)).zip(Seq(knoSet, distinct_pool))) {
        plot.zera()
        if (unl == distinct_pool) {
          for (p <- unl) plot.bola(p.x, p.y, m.predict(p).toInt, 9)
        } else {
          for (p <- unl) {
            plot.bola_intense(p.x, p.y, m.distribution(p).head.toFloat, 9)
            //            println(m.distribution(p).head)
          }
        }
        for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
        if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
        plot.mostra()
      }
    Thread.sleep((delay * 1000).round.toInt)
  }
}
