/*
mls: basic machine learning algorithms for Scala
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
package ml.classifiers

import clean.lib.Ds
import ml.Pattern
import ml.models.Model

case class BestClassif(ds: Ds, seed: Int, pool: Seq[Pattern]) extends Learner {
   override lazy val toString = s"BestLearner: $ds"
   lazy val id = learner.id
   lazy val abr: String = learner.abr
   lazy val attPref: String = learner.attPref
   lazy val boundaryType: String = learner.boundaryType
   lazy val learners = Seq(
      KNNBatcha(5, "eucl", pool, weighted = true)
      , C45()
      , RF(seed)
      , NBBatch()
      , CIELMBatch(seed)
      , SVMLibRBF(seed)
   )
   lazy val bestLearnerId = ds.bestLearnerId
   lazy val learner = learners.find(_.id == bestLearnerId).get
   override lazy val querFiltro = qf(learner)

   def update(model: Model, fast_mutable: Boolean, semcrescer: Boolean)(pattern: Pattern) = learner.update(model, fast_mutable, semcrescer)(pattern)

   def expected_change(model: Model)(pattern: Pattern) = learner.expected_change(model)(pattern)

   def build(pool: Seq[Pattern]) = learner.build(pool)
}
