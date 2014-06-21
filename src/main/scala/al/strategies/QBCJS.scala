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

/**
 * If the Learner does not generate an ensemble,
 * QBCJS performs exactly like RandomSampling,
 * since the divergence will be always zero.
 * If the generated ensemble mambers offer only "hard" distributions (1,0,0),
 * QBCJS performs exactly like MarginSampling (only empirically confirmed).
 * @param learner
 * @param pool
 * @param debug
 */
case class QBCJS(learner: Learner, pool: Seq[Pattern], debug: Boolean = false)
  extends StrategyWithLearner with EntropyMeasure with JSMeasure {
  override val toString = "QBCJS"

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    ??? //todo: implement EnsembleModel
    //      println("current_model.distributions(unlabeled.head) = " + current_model.distributions(unlabeled.head).map(_.toList).toList)
    //unlabeled maxBy (pa => JSdivergence(current_model.distributions(pa)))
    pool.head
  }
}