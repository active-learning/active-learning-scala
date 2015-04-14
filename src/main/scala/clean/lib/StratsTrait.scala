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
   def stratsTex(dist: String) = {
      val fakePool = Seq()
      Seq(
         Some((learner: Learner) => RandomSampling(fakePool)) //0
         , Some((learner: Learner) => ClusterBased(fakePool)) //1
         , if (dist == "eucl" || dist == "all") Some((learner: Learner) => AgDensityWeightedTrainingUtility(fakePool, "eucl")) else None
         , if (dist == "manh" || dist == "all") Some((learner: Learner) => AgDensityWeightedTrainingUtility(fakePool, "manh")) else None
         , if (dist == "maha" || dist == "all") Some((learner: Learner) => AgDensityWeightedTrainingUtility(fakePool, "maha")) else None
         , if (dist == "eucl" || dist == "all") Some((learner: Learner) => HTUFixo(fakePool, learner, fakePool, "eucl")) else None
         , if (dist == "manh" || dist == "all") Some((learner: Learner) => HTUFixo(fakePool, learner, fakePool, "manh")) else None
         , if (dist == "maha" || dist == "all") Some((learner: Learner) => HTUFixo(fakePool, learner, fakePool, "maha")) else None
         , Some((learner: Learner) => SGmultiFixo(learner, fakePool, "consensus"))
         , Some((learner: Learner) => EntropyFixo(learner, fakePool))
         , Some((learner: Learner) => MarginFixo(learner, fakePool))
         , if (dist == "eucl" || dist == "all") Some((learner: Learner) => DensityWeightedFixo(fakePool, learner, fakePool, 1, "eucl")) else None
         , if (dist == "manh" || dist == "all") Some((learner: Learner) => DensityWeightedFixo(fakePool, learner, fakePool, 1, "manh")) else None
         , if (dist == "maha" || dist == "all") Some((learner: Learner) => DensityWeightedFixo(fakePool, learner, fakePool, 1, "maha")) else None
         , if (dist == "eucl" || dist == "all") Some((learner: Learner) => DensityWeightedTrainingUtilityFixo(fakePool, learner, fakePool, "eucl")) else None
         , if (dist == "manh" || dist == "all") Some((learner: Learner) => DensityWeightedTrainingUtilityFixo(fakePool, learner, fakePool, "manh")) else None
         , if (dist == "maha" || dist == "all") Some((learner: Learner) => DensityWeightedTrainingUtilityFixo(fakePool, learner, fakePool, "maha")) else None
         , Some((learner: Learner) => ExpErrorReductionMarginFixo(learner, fakePool, "balacc"))
         , Some((learner: Learner) => ExpErrorReductionMarginFixo(learner, fakePool, "entropy"))
         , Some((learner: Learner) => SVMmultiRBF(fakePool, "BALANCED_EEw"))
         , Some((learner: Learner) => SVMmultiRBF(fakePool, "SIMPLEw"))
      ).flatten
   }

   /*
   % procedimento pra transformar strat em Fixo
   % copy
   % tostr
   % abr+learner
   % muda id e +convlid
   % põe no strats
   % põe id na lista do Global (não tem mais lista.drop no Ds.scala)
    */
   def stratsPool(dist: String, poolForLearner: Seq[Pattern] = Seq(), pool: Seq[Pattern] = Seq()) =
   //   //essas ganharam ids por par s/l porque suas medidas de distancia foram afetadas por filtros
   //      (dist match {
   //         case "man" => Seq((learner: Learner) => DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "manh")
   //            , (learner: Learner) => HTUFixo(poolForLearner, learner, pool, "manh")
   //            , (learner: Learner) => DensityWeightedFixo(poolForLearner, learner, pool, 1, "manh")
   //            , (learner: Learner) => AgDensityWeightedTrainingUtility(pool, "manh") //791
   //         )
   //         case "euc" => Seq((learner: Learner) => DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "eucl")
   //            , (learner: Learner) => HTUFixo(poolForLearner, learner, pool, "eucl")
   //            , (learner: Learner) => DensityWeightedFixo(poolForLearner, learner, pool, 1, "eucl")
   //            , (learner: Learner) => AgDensityWeightedTrainingUtility(pool, "eucl") //691
   //         )
   //         case "all" => Seq((learner: Learner) => DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "manh")
   //            , (learner: Learner) => HTUFixo(poolForLearner, learner, pool, "manh")
   //            , (learner: Learner) => DensityWeightedFixo(poolForLearner, learner, pool, 1, "manh")
   //            , (learner: Learner) => AgDensityWeightedTrainingUtility(pool, "manh") //791
   //            , (learner: Learner) => DensityWeightedTrainingUtilityFixo(poolForLearner, learner, pool, "eucl")
   //            , (learner: Learner) => HTUFixo(poolForLearner, learner, pool, "eucl")
   //            , (learner: Learner) => DensityWeightedFixo(poolForLearner, learner, pool, 1, "eucl")
   //            , (learner: Learner) => AgDensityWeightedTrainingUtility(pool, "eucl") //691
   //         )
   //         case "mah" => Seq()
   //      }) ++
      Seq(
         (learner: Learner) => QBC(poolForLearner)
         //         //essas precisam ser Fixo porque os hits ficam sem vinculo com o learner gerador das queries (por isso dava duplicated key)
         //         //copiei as qs e os hs porque todos já estavam gerados desde antigamente
         //         ,(learner: Learner) => EntropyFixo(learner, poolForLearner) //
         //         , (learner: Learner) => MarginFixo(learner, poolForLearner) //pid:100000 ... 100050; sid:3000000 ... 3000050
         //         , (learner: Learner) => ExpErrorReductionMarginFixo(learner, poolForLearner, "entropy") //pid:200000 ... 200050; sid:11000000 ...          //11000050
         //         , (learner: Learner) => ExpErrorReductionMarginFixo(learner, poolForLearner, "balacc") //pid:300000 ... 300050; sid:74000000 ... 74000050
         //         , (learner: Learner) => SGmultiFixo(learner, poolForLearner, "consensus") //pid:400000 ... 400050; sid:14000000 ... 14000050
         //
         //         //essas naturalmente não usaram filtro e cada 'learner' decidiu sozinho se usava filtro ou não (all.scala mostra que hits foi feito c/s filtro de acordo com classif)
         //         //(svm.scala força filtro nas strats, porém, por serem agnósticas, as queries já foram geradas corretamente antes pelo all.scala ou rf.scala
         //         , (learner: Learner) => RandomSampling(pool) //0
         //         , (learner: Learner) => ClusterBased(pool) //1
      )

   def stratsFpool(poolForLearner: Seq[Pattern] = Seq(), fpool: Seq[Pattern] = Seq()) = Seq(
      //      //essas ganharam ids por par s/l porque medem distancia filtradas e afetaram seus learners (e precisavam ser reimplementadas para receber pools independentes)
      //      (learner: Learner) => DensityWeightedTrainingUtilityFixo(poolForLearner, learner, fpool, "maha")
      //      , (learner: Learner) => HTUFixo(poolForLearner, learner, fpool, "maha")
      //      , (learner: Learner) => DensityWeightedFixo(poolForLearner, learner, fpool, 1, "maha")
      //
      //      //essa strat pede filtro, então forçou filtro no classif (que era chamado de learner)
      //      //apaguei somente learners que não querem filtro no mysql: id voltou pra 991
      //      , (learner: Learner) => AgDensityWeightedTrainingUtility(fpool, "maha") //991

      //apaguei todos os hits de classifs diferentes de svmrbf.
      (learner: Learner) => SVMmultiRBF(fpool, "BALANCED_EEw") //9690094
      , (learner: Learner) => SVMmultiRBF(fpool, "SIMPLEw") //9660091
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
      //      Entropy(learner, pool) //4
      MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "eucl") //5
      , DensityWeightedFixo(Seq(), learner, pool, 1, "manh") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "eucl") //7
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "manh") //7

      , SGmultiFixo(learner, pool, "consensus") //14
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11
      , HTUFixo(Seq(), learner, pool, "eucl") //4003006
      , HTUFixo(Seq(), learner, pool, "manh") //4003007
   )

   def stratsComLearnerExterno_FilterDependent(pool: Seq[Pattern], learner: Learner) = List[Strategy](
      DensityWeightedFixo(Seq(), learner, pool, 1, "maha") //9
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "maha") //9
      , HTUFixo(Seq(), learner, pool, "maha") //4003009
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
      , HTUFixo(Seq(), learner, pool, "eucl") //4003006
      , HTUFixo(Seq(), learner, pool, "manh") //4003007
      , HTUFixo(Seq(), learner, pool, "maha") //4003009
      , new SGmultiFixo(learner, pool, "consensus") //14
      //      , Entropy(learner, pool) //4
      , MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "eucl") //5
      , DensityWeightedFixo(Seq(), learner, pool, 1, "manh") //5
      , DensityWeightedFixo(Seq(), learner, pool, 1, "maha") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "eucl")
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "manh")
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "maha") //9
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeRedux(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      , AgDensityWeightedTrainingUtility(pool, "maha") //901
      , HTUFixo(Seq(), learner, pool, "eucl") //4003006
      , HTUFixo(Seq(), learner, pool, "manh") //4003007
      , HTUFixo(Seq(), learner, pool, "maha") //4003009
      , new SGmultiFixo(learner, pool, "consensus") //14
      //      , Entropy(learner, pool) //4
      , MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "eucl") //5
      , DensityWeightedFixo(Seq(), learner, pool, 1, "manh") //5
      , DensityWeightedFixo(Seq(), learner, pool, 1, "maha") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "eucl")
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "manh")
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "maha") //9
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxEuc(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "eucl") //601
      , HTUFixo(Seq(), learner, pool, "eucl") //4003006
      , new SGmultiFixo(learner, pool, "consensus") //14
      //      , Entropy(learner, pool) //4
      , MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "eucl") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "eucl")
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxMan(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "manh") //701
      , HTUFixo(Seq(), learner, pool, "manh") //4003009
      , new SGmultiFixo(learner, pool, "consensus") //14
      //      , Entropy(learner, pool) //4
      , MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "manh") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "manh") //9
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11

      , SVMmultiRBF(pool, "BALANCED_EEw")
      , SVMmultiRBF(pool, "SIMPLEw")
      , ExpELMChange(pool), //1006600
      QBC(pool) //1292212
   )

   def stratsForTreeReduxMah(pool: Seq[Pattern] = Seq(), learner: Learner = NoLearner()) = Seq(
      RandomSampling(pool) //0
      , ClusterBased(pool) //1
      , AgDensityWeightedTrainingUtility(pool, "maha") //901
      , HTUFixo(Seq(), learner, pool, "maha") //4003009
      , new SGmultiFixo(learner, pool, "consensus") //14
      //      , Entropy(learner, pool) //4
      , MarginFixo(learner, pool) //3
      , DensityWeightedFixo(Seq(), learner, pool, 1, "maha") //5
      , DensityWeightedTrainingUtilityFixo(Seq(), learner, pool, "maha") //9
      , ExpErrorReductionMarginFixo(learner, pool, "balacc") //74
      , ExpErrorReductionMarginFixo(learner, pool, "entropy") //11

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