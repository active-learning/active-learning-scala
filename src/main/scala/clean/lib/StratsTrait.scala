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

package clean.lib

import al.strategies._
import ml.Pattern
import ml.classifiers.{Learner, NoLearner}

trait StratsTrait {
   def stratsPool(poolForLearner: Seq[Pattern], learner: Learner, pool: Seq[Pattern]) = Seq(
      DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "manh")
      , DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "eucl")
      , MarginFixo(learner, poolForLearner)
      , ExpErrorReductionMarginFixo(learner, poolForLearner, "entropy")
      , HTUFixo(poolForLearner, learner, pool, "manh")
      , HTUFixo(poolForLearner, learner, pool, "eucl")
      , SGmultiFixo(learner, poolForLearner, "consensus")
      // //   , DensityWeightedFixo(poolForLearner, learner, pool, 1, "eucl")
   )

   def stratsFpool(poolForLearner: Seq[Pattern], learner: Learner, fpool: Seq[Pattern]) = Seq(
      DensityWeightedTrainingUtilityFixo(poolForLearner, learner, fpool, "maha")
      , HTUFixo(poolForLearner, learner, fpool, "maha")
      , AgDensityWeightedTrainingUtility(fpool, "maha")
      , SVMmultiRBF(fpool, "BALANCED_EEw")
      , SVMmultiRBF(fpool, "SIMPLEw")
   )

   def allStrats(learner: Learner = NoLearner(), pool: Seq[Pattern] = Seq()) = stratsemLearnerExterno(pool) ++ stratcomLearnerExterno(learner, pool)

   def stratcomLearnerExterno(learner: Learner = NoLearner(), pool: Seq[Pattern] = Seq()) = stratsComLearnerExterno_FilterFree(pool, learner) ++ stratsComLearnerExterno_FilterDependent(pool, learner)

   def stratsemLearnerExterno(pool: Seq[Pattern] = Seq()) = stratsSemLearnerExterno_FilterFree(pool) ++ stratsSemLearnerExterno_FilterDependent(pool)

   //   def stratsSGmajJS(pool: Seq[Pattern], learner: Learner) = List[Strategy](new SGmulti(learner, pool, "majority"), new SGmultiJS(learner, pool))


   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////

   def stratsSemLearnerExterno_FilterFree(pool: Seq[Pattern]) = List[Strategy](
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      , QBC(pool) //1292212
   )

   def stratsSemLearnerExterno_FilterDependent(pool: Seq[Pattern]) = List[Strategy](
      AgDensityWeightedTrainingUtility(pool, "maha"), //901
      //TROQUEI LEARNER DO SVMmultiLinear PARA SVMLibDegree1
      // (aproveitando id/queries das estratégias SVMmulti),
      // demora mais que LibLinear, mas fica em linha com artigo do Tong!
      //      SVMmultiLinear(pool, "BALANCED_EEw"),
      //      SVMmultiLinear(pool, "SIMPLEw"),
      //      SVMmultiRBFW(pool, "BALANCED_EEw"),
      //      SVMmultiRBFW(pool, "SIMPLEw"),
      SVMmultiRBF(pool, "BALANCED_EEw"),
      SVMmultiRBF(pool, "SIMPLEw"),
      ExpELMChange(pool) //1006600
   )

   def stratsComLearnerExterno_FilterFree(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl") //7
      , DensityWeightedTrainingUtility(learner, pool, "manh") //7
      , new SGmulti(learner, pool, "consensus") //14
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11
      , HTU(learner, pool, "eucl") //4003006
      , HTU(learner, pool, "manh") //4003007
   )

   def stratsComLearnerExterno_FilterDependent(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , HTU(learner, pool, "maha") //4003009
   )

   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////

   //   val stratsForTreeSemSVM = stratsForTree().take(7) ++ stratsForTree().drop(9)
   //   val stratsForTreeSemSVMRedux = stratsForTreeRedux().take(5) ++ stratsForTreeRedux().drop(7)

   def stratsForTree(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      , AgDensityWeightedTrainingUtility(pool, "maha") //901
      , HTU(learner, pool, "eucl") //4003006
      , HTU(learner, pool, "manh") //4003007
      , HTU(learner, pool, "maha") //4003009
      , new SGmulti(learner, pool, "consensus") //14
      , Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl")
      , DensityWeightedTrainingUtility(learner, pool, "manh")
      , DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeRedux(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl")
      , AgDensityWeightedTrainingUtility(pool, "manh")
      , AgDensityWeightedTrainingUtility(pool, "maha")
      , HTU(learner, pool, "eucl")
      , HTU(learner, pool, "manh")
      , HTU(learner, pool, "maha")
      , new SGmulti(learner, pool, "consensus") //14
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl")
      , DensityWeightedTrainingUtility(learner, pool, "manh")
      , DensityWeightedTrainingUtility(learner, pool, "maha")
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxEuc(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl")
      , HTU(learner, pool, "eucl")
      , new SGmulti(learner, pool, "consensus") //14
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl")
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxMan(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "manh")
      , HTU(learner, pool, "manh")
      , new SGmulti(learner, pool, "consensus") //14
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "manh")
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxMah(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "maha")
      , HTU(learner, pool, "maha")
      , new SGmulti(learner, pool, "consensus") //14
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "maha")
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )
}

/* ids:
      Majoritary(pool) //21
      , RandomSampling(pool) //0
      , ClusterBased(pool) //1
      Uncertainty(learner, pool) //2
      , Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeighted(learner, pool, 0.5, "eucl") //5005
      , AgDensityWeightedTrainingUtility(learner, pool, "eucl") //600
      , AgDensityWeightedLabelUtility(learner, pool, "eucl") //360
      , AgDensityWeightedTrainingUtility(learner, pool, "eucl", 0.5, 0.5) //650
      , AgDensityWeightedLabelUtility(learner, pool, "eucl", 0.5, 0.5) //410
      , DensityWeightedTrainingUtility(learner, pool, "eucl") //6
      , DensityWeightedTrainingUtility(learner, pool, "manh") //7
      , DensityWeightedLabelUtility(learner, pool, "eucl") //36
      , DensityWeightedTrainingUtility(learner, pool, "eucl", 0.5, 0.5) //50006
      , DensityWeightedTrainingUtility(learner, pool, "manh", 0.5, 0.5) //50007
      , DensityWeightedLabelUtility(learner, pool, "eucl", 0.5, 0.5) //86
      , ExpErrorReductionMargin(learner, pool, "entropy") //11
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , new SGmulti(learner, pool, "consensus") //14
      , new SGmulti(learner, pool, "majority") //15
      DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , DensityWeightedLabelUtility(learner, pool, "maha") //39
      , DensityWeightedTrainingUtility(learner, pool, "maha", 0.5, 0.5) //50009
      , DensityWeightedLabelUtility(learner, pool, "maha", 0.5, 0.5) //89
  */