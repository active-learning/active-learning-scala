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

package ml.classifiers

import ml.Pattern
import ml.models.Model

/**
 * Created by davi on 10/06/14.
 */
case class NoLearner() extends Learner {
  override val toString = "NoLearner"
  val id = 0

  def EMC(model: Model)(patterns: Seq[Pattern]) = ???

  def update(model: Model, fast_mutable: Boolean)(pattern: Pattern) = ???

  def expected_change(model: Model)(pattern: Pattern) = ???

  /**
   * Every call to build generates a model from scratch
   * (and reinstanciate all needed internal mutable objects, if any).
   * @param pool
   * @return
   */
  def build(pool: Seq[Pattern]) = ???
}
