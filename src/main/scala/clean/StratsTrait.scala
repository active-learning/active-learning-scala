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

package clean

import al.strategies._
import ml.Pattern
import ml.classifiers.{NoLearner, Learner}

trait StratsTrait {
  def allStrats(learner: Learner = NoLearner(), pool: Seq[Pattern] = Seq()) = stratsemLearnerExterno(pool) ++ stratcomLearnerExterno(learner, pool)

   def stratcomLearnerExterno(learner: Learner = NoLearner(), pool: Seq[Pattern] = Seq()) = stratsComLearnerExterno_FilterFree(pool, learner) ++ stratsComLearnerExterno_FilterDependent(pool, learner)

   def stratsemLearnerExterno(pool: Seq[Pattern] = Seq()) = stratsSemLearnerExterno(pool)

   def stratsSemLearnerExterno(pool: Seq[Pattern]) = List[Strategy](
    //    Majoritary(pool)
    RandomSampling(pool)
    , ClusterBased(pool)
    , SVMmulti(pool, "BALANCED_EE") //SIMPLE + KFF
    //    ,SVMmulti(pool, "SIMPLE") // exploitation="uncertainty"
    //    ,SVMmulti(pool, "KFF") // exploration
      , SVMmulti(pool, "SELF_CONF") //EER
  )

   def stratsComLearnerExterno_FilterFree(pool: Seq[Pattern], learner: Learner) = List[Strategy](
    //    Uncertainty(learner, pool),
      Entropy(learner, pool)
      , Margin(learner, pool)
    , DensityWeighted(learner, pool, 1, "eucl")
    , DensityWeightedTrainingUtility(learner, pool, "cheb")
      , DensityWeightedTrainingUtility(learner, pool, "eucl")
      , DensityWeightedTrainingUtility(learner, pool, "manh")

    , DensityWeightedLabelUtility(learner, pool, "eucl")
    , DensityWeightedLocalUtility(learner, pool, "eucl")
    //    , DensityWeightedLocalLabelUtility(learner, pool, "eucl") ??? corrigir 1out of bounds!

    , ExpErrorReductionMargin(learner, pool, "entropy")
      , ExpErrorReductionMargin(learner, pool, "gmeans+residual")
      , ExpErrorReductionMargin(learner, pool, "accuracy")
    , new SGmulti(learner, pool, "consensus")
    //    , new SGmulti(learner, pool, "majority")
    //    new SGmultiJS(learner, pool)
  )

   def stratsComLearnerExterno_FilterDependent(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      DensityWeightedTrainingUtility(learner, pool, "maha")
    //      ,DensityWeightedLabelUtility(learner, pool, "maha")
    //      ,DensityWeightedLocalUtility(learner, pool, "maha")
  )
}
