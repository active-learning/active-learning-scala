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

package clean.run

import al.strategies._
import clean.nonQ
import ml.Pattern

object fast extends nonQ {
  val context = "fastApp"
  init()

  def strats(pool: Seq[Pattern], learnerSeed: Int) = List(
    RandomSampling(pool),
    ClusterBased(pool),
    Uncertainty(learner(pool, learnerSeed), pool),
    Entropy(learner(pool, learnerSeed), pool),
    Margin(learner(pool, learnerSeed), pool),
    DensityWeighted(learner(pool, learnerSeed), pool, 1, "eucl"),
    DensityWeightedTrainingUtility(learner(pool, learnerSeed), pool, "cheb"),
    DensityWeightedTrainingUtility(learner(pool, learnerSeed), pool, "eucl"),
    DensityWeightedTrainingUtility(learner(pool, learnerSeed), pool, "maha"),
    DensityWeightedTrainingUtility(learner(pool, learnerSeed), pool, "manh"),
    MahalaWeightedTrainingUtility(learner(pool, learnerSeed), pool, 1, 1),
    new SGmulti(learner(pool, learnerSeed), pool, "consensus"),
    new SGmulti(learner(pool, learnerSeed), pool, "majority"),
    new SGmultiJS(learner(pool, learnerSeed), pool)
    //      SVMmulti(pool, "SELF_CONF"),
    //    SVMmulti(pool, "KFF"),
    //    SVMmulti(pool, "BALANCED_EE"),
    //    SVMmulti(pool, "SIMPLE")
  )
}
