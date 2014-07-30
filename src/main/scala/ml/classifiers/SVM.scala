package ml.classifiers

import ml.Pattern
import ml.models.Model
import svmal.libsvm.SvmLib

/*
elm-scala: an implementation of ELM in Scala using MTJ
Copyright (C) 2014 Davi Pereira dos Santos

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
/**
 * SVM convencional (batch training)
 */
case class SVM() extends Learner {
  override val toString = "SVM"
  val notes = ""
  val svm = new SvmLib

  def update(model: Model, fast_mutable: Boolean)(pattern: Pattern) = ???

  def expected_change(model: Model)(pattern: Pattern) = ???

  def EMC(model: Model)(patterns: Seq[Pattern]) = ???

  /**
   * Every call to build generates a model from scratch
   * (and reinstanciate all needed internal mutable objects, if any).
   * @param pool
   * @return
   */
  def build(pool: Seq[Pattern]) = {
    svm.buildClassifier(pool.head.dataset())
    ???
  }
}
