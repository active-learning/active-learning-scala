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

trait StrategyWithLearner extends Strategy {
  def learner: Learner

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val current_model = learner.build(labeled)
    queries_rec(current_model, unlabeled, labeled)
  }

  private def queries_rec(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      if (debug) visual_test(null, unlabeled, labeled)
      val selected = next(current_model, unlabeled, labeled)
      val new_model = learner.update(current_model, fast_mutable = true)(selected)
      if (debug) visual_test(selected, unlabeled, labeled)
      selected #:: queries_rec(new_model, unlabeled.diff(Seq(selected)), labeled :+ selected)
    }
  }

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    val current_model = learner.build(labeled)
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, current_model.predict(p).toInt, 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
    plot.mostra()
    Thread.sleep((delay * 1000).round.toInt)
  }
}
