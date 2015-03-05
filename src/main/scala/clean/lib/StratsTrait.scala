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
   )

   def stratsSemLearnerExterno_FilterDependent(pool: Seq[Pattern]) = List[Strategy](
      AgDensityWeightedTrainingUtility(pool, "maha"), //901
      //TROCAR LEARNER DO SVMmultiLinear PARA SVMLibDegree1
      // (aproveitando id/queries das 4 estratégias),
      // demora mais que LibLinear, mas ficaria em linha com artigo do Tong!
      SVMmultiLinear(pool, "KFFw"),
      SVMmultiLinear(pool, "BALANCED_EEw"),
      SVMmultiLinear(pool, "SIMPLEw"),
      SVMmultiLinear(pool, "SELF_CONFw"),
      ExpELMChange(pool), //1006600
      QBC(pool) //292212
      //DESCOMENTAR ABAIXO PARA TESTAR SVM COM RBF
      //      SVMmultiRBF(pool, "KFFw"),
      //      SVMmultiRBF(pool, "BALANCED_EEw"),
      //      SVMmultiRBF(pool, "SIMPLEw"),
      //      SVMmultiRBF(pool, "SELF_CONFw")
   )

   def stratsComLearnerExterno_FilterFree(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl") //7
      , DensityWeightedTrainingUtility(learner, pool, "manh") //7
      , new SGmulti(learner, pool, "consensus") //14
      , new SGmulti(learner, pool, "majority") //15
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11
      , GATU(learner, pool, "eucl") //4003006
      , GATU(learner, pool, "manh") //4003007
   )

   def stratsComLearnerExterno_FilterDependent(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , GATU(learner, pool, "maha") //4003009
   )

   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   //////////////////////////////////////////////////////////////////////////////////////////////////////////////

   //   val stratsForTreeSemSVM = stratsForTree().take(7) ++ stratsForTree().drop(9)
   //   val stratsForTreeSemSVMRedux = stratsForTreeRedux().take(5) ++ stratsForTreeRedux().drop(7)

   def stratsForTree(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , ExpErrorReductionMargin(learner, pool, "entropy") //11
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , new SGmulti(learner, pool, "consensus") //14
      , new SGmulti(learner, pool, "majority") //15
      , DensityWeighted(learner, pool, 1, "eucl") //5
      //      , DensityWeightedTrainingUtility(learner, pool, "eucl") //6
      //      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      //      , DensityWeightedLabelUtility2(learner, pool, "eucl") //36
      //      , AgDensityWeightedLabelUtility2(pool, "eucl") //361
      , DensityWeightedTrainingUtility(learner, pool, "manh") //7
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      //      , DensityWeightedLabelUtility2(learner, pool, "manh") //37
      //      , AgDensityWeightedLabelUtility2(pool, "manh") //371
      , DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , AgDensityWeightedTrainingUtility(pool, "maha") //901  //tava comentado
      //      , DensityWeightedLabelUtility2(learner, pool, "maha") //39
      //      , AgDensityWeightedLabelUtility2(pool, "maha") //391
      , GATU(learner, pool, "manh") //
      , GATU(learner, pool, "maha") //
   )

   def stratsForTreeRedux(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      , AgDensityWeightedTrainingUtility(pool, "maha") //901
      , Entropy(learner, pool) //4
      , Margin(learner, pool) //3
      , DensityWeighted(learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtility(learner, pool, "eucl") //7
      , DensityWeightedTrainingUtility(learner, pool, "manh") //7
      , new SGmulti(learner, pool, "consensus") //14
      , new SGmulti(learner, pool, "majority") //15
      , ExpErrorReductionMargin(learner, pool, "balacc") //74
      , ExpErrorReductionMargin(learner, pool, "entropy") //11
      , GATU(learner, pool, "eucl") //4003006
      , GATU(learner, pool, "manh") //4003007
      , DensityWeightedTrainingUtility(learner, pool, "maha") //9
      , GATU(learner, pool, "maha") //4003009
      , SVMmultiLinear(pool, "KFFw")
      , SVMmultiLinear(pool, "BALANCED_EEw")
      , SVMmultiLinear(pool, "SIMPLEw")
      , SVMmultiLinear(pool, "SELF_CONFw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //292212
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