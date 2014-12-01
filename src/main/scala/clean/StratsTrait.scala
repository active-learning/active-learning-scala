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
      //      Majoritary(pool)
      //      , RandomSampling(pool)
      //      , ClusterBased(pool)
      //      ,
      //      SVMmulti(pool, "BALANCED_EEw") //SIMPLE + KFF
   )

   def stratsSGmajJS(pool: Seq[Pattern], learner: Learner) = List[Strategy](new SGmulti(learner, pool, "majority"), new SGmultiJS(learner, pool))

   def stratsComLearnerExterno_FilterFree(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      //      Uncertainty(learner, pool)
      //      , Entropy(learner, pool)
      //      , Margin(learner, pool)
      //      , DensityWeighted(learner, pool, 1, "eucl")
      //      , DensityWeighted(learner, pool, 0.5, "eucl")

      AgDensityWeightedTrainingUtility(learner, pool, "eucl")
      , AgDensityWeightedLabelUtility(learner, pool, "eucl")
      , AgDensityWeightedTrainingUtility(learner, pool, "eucl", 0.5)
      , AgDensityWeightedLabelUtility(learner, pool, "eucl", 0.5)
      //
      //      , DensityWeightedTrainingUtility(learner, pool, "eucl")
      //      , DensityWeightedTrainingUtility(learner, pool, "manh")
      //      , DensityWeightedLabelUtility(learner, pool, "eucl")
      //      , DensityWeightedTrainingUtility(learner, pool, "eucl", 0.5)
      //      , DensityWeightedTrainingUtility(learner, pool, "manh", 0.5)
      //      , DensityWeightedLabelUtility(learner, pool, "eucl", 0.5)
      //
      //      , ExpErrorReductionMargin(learner, pool, "entropy")
      //      , ExpErrorReductionMargin(learner, pool, "balacc")
      //      , new SGmulti(learner, pool, "consensus")
      //      , new SGmulti(learner, pool, "majority")
   )

   def stratsComLearnerExterno_FilterDependent(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      //      DensityWeightedTrainingUtility(learner, pool, "maha")
      //      , DensityWeightedLabelUtility(learner, pool, "maha")
      //      , DensityWeightedTrainingUtility(learner, pool, "maha", 0.5)
      //      , DensityWeightedLabelUtility(learner, pool, "maha", 0.5)
   )
}

//0 rnd
//1 clu
//2 unc
//3 mar
//4 ent
//5 DW
//6 DWTUeu
//7 DWTUman
//8 DWTUche
//9 DWTUmah
//10
//11 eer ent
//12 eer acc
//13 eer gme
//14 sg con
//15 sg maj
//17 simple
//18 self
//19 kff
//20 balanced
//21 maj

//36 dwlau eu
//
//46 dwlou eu
//74 eer balacc