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
import ml.classifiers.Maj
import ml.models.Model

case class Majoritary(pool: Seq[Pattern], debug: Boolean = false)
  extends StrategyWithLearner {
  override val toString = "Random Samplingj"
  val abr = "Rndj"
  val id = 21

  def learner = Maj()

  protected def next(model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    unlabeled.head
  }
}

