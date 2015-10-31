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
import ml.models.{WekaBatModel2, WekaBatModel, Model}
import util.Graphics.Plot

trait StrategySGmulti extends Strategy {
  lazy val background_weight = 1 / (distinct_pool.length.toDouble * nclasses)
  //The more classes we have, bigger is the disagreement; we have to compensate reducing the artificial weights.
  lazy val plots = Array.fill(nclasses)(new Plot)
  lazy val models_to_visualize = new Array[Model](nclasses)

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    val specificPoolsPlusLabeled = {
      for (c <- 0 until nclasses) yield (firstof_each_class ++ (distinct_pool map (_.relabeled_reweighted(c, background_weight, new_missed = false))) ++ labeled.drop(firstof_each_class.size)).toList
    }.toArray
    val current_models = specificPoolsPlusLabeled map learner.build
    //    val initial_models = specific_pools map learner.build
    //    val current_models = initial_models map (m => learner.updateAll(m, fast_mutable = true)(labeled))
    queries_rec(current_models, unlabeled, labeled)
  }

  private def queries_rec(current_models: Array[Model], unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      if (debug) visual_test(null, unlabeled, labeled)
      val selected = controversial(unlabeled, current_models)

      val new_models = learner.id match {
        case 2651110 | 556665 | 165111 => //SVM não lida bem com exemplos duplicados, suponho que tire a média; então temos que tirar o antigo e por o novo com o peso integral; suponho que ELMs não tenham problema
          current_models map { m =>
            val wm = m.asInstanceOf[WekaBatModel]
            val newTR = selected +: wm.training_set.diff(Seq(selected)) //troca por ele mesmo mas com peso integral
            learner.build(newTR) //assume batch learning
          }
        case x if x >= 100000000 => //ML
          current_models map { m =>
            val wm = m.asInstanceOf[WekaBatModel2]
            val newTR = selected +: wm.labeled.diff(Seq(selected)) //troca por ele mesmo mas com peso integral
            learner.build(newTR)
          }
        case _ =>
          //Update specific models with queried label.
          //The real weight (1) is far bigger than the artificial weight(<< 0.01).
          //Therefore, instead of a total retraining, just a incremental retraining with the correct label/weight should suffice.
          current_models map (m => learner.update(m, fast_mutable = true)(selected))
      }

      if (debug) {
        new_models.copyToArray(models_to_visualize)
        visual_test(selected, unlabeled, labeled)
      }
      selected #:: queries_rec(new_models, unlabeled.diff(Seq(selected)), labeled :+ selected)
      }
  }

  protected def most_votes(votes: Array[Int]) = votes.groupBy(identity).map(_._2.size).max

  def controversial(unlabeled: Seq[Pattern], current_models: Array[Model]): Pattern

  def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern])
}

