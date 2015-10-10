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

import ml.{classifiers, Pattern}
import ml.classifiers._

trait LearnerTrait {
  def learners(learnersStr: Seq[String]) = learnersStr map str2learner()

  def allLearners(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed)

  //neste arquivo ficam apenas classificadores básicos.
  def learnersPool(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = List[Learner](
    //individuais, 4 tipos
    KNNBatcha(5, "eucl", pool, weighted = true)
    , KNNBatcha(5, "eucl", pool, weighted = false)
    , KNNBatcha(5, "manh", pool, weighted = true)
    , KNNBatcha(5, "manh", pool, weighted = false)
    , NBBatch()
    , C452()
    //SVM usa filtro, então aparece mais abaixo, na outra função

    //Florestas, 3 tipos distintos de ensemble
    , RF(learnerSeed)
    , ABoo(learnerSeed)
    //RoF usa filtro, então aparece mais abaixo, na outra função

    //Baggings e 10NN
    , BagC45(learnerSeed)
    , BagNB(learnerSeed)
    , Knn10(pool, weighted = true)
    , Knn10(pool, weighted = false)
  )

  def learnersFpool(learnerSeed: Int = -1) = List[Learner](
    RoF(learnerSeed)
    , SVMLibRBF(learnerSeed)
  )

  def learnersFilterFree(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = List[Learner](
    KNNBatcha(5, "eucl", pool, weighted = true) //2
    , C45() //3
    //      , RF(learnerSeed) //773
    , NBBatch() //12
    //      , VFDT() //4
  )

  def learnersFilterDependent(learnerSeed: Int = -1) = List[Learner](
    //      , NinteraELM(learnerSeed) //11
    ////      , LogReg(learnerSeed) //556665
    //      , SVMLibDegree1(learnerSeed)//165111
    ////      , SVMLibRBF(learnerSeed)
  )

  def str2learner(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1)(str: String) = str match {
    case "bagnb" => BagNB(learnerSeed)
    case "bagc45" => BagC45(learnerSeed)
    case "10nn" => Knn10(pool)

    case "aboo" => ABoo(learnerSeed)
    case "rof" => RoF(learnerSeed)
    case "nbb" => NBBatch()
    case "5nna" => KNNBatcha(5, "eucl", pool, weighted = true)
    case "5nnb" => KNNBatchb(5, "eucl", pool, weighted = true)
    case "c45" => C45()
    case "c452" => C452()
    case "rf" => RF(learnerSeed)
    case "vfdt" => VFDT()
    //      case "reg" => LogReg(learnerSeed)
    case "poly1" => SVMLibDegree1(learnerSeed)
    case "rbf" => SVMLibRBF(learnerSeed)
    //      case "nb" => NB()
    //      case "ci" => CIELM(learnerSeed)
    //      case "i" => IELM(learnerSeed)
    //      case "maj" => Maj()
    //      case "eci" => ECIELM(learnerSeed)
    //      case "ib" => IELMBatch(learnerSeed)
    //      case "ei" => EIELM(learnerSeed)
    //      case "intera" => interaELM(learnerSeed)

    //      case "NBz" => NB("")
    //      case "C45z" => C45("")
    //      case "VFDTz" => VFDT("")
    //      case "LASVM" => LASVM()
    //      case "1NNc" => KNNBatch(1, "cheb", pool)
    //      case "1NNe" => KNNBatch(1, "eucl", pool)
    //      case "1NNm" => KNNBatch(1, "manh", pool)
    //      case "3NNc" => KNNBatch(3, "cheb", pool)
    //      case "3NNe" => KNNBatch(3, "eucl", pool, "", weighted = true)
    //      case "3NNm" => KNNBatch(3, "manh", pool)
    //      case "5NNc" => KNNBatch(5, "cheb", pool)
    //      case "5NNm" => KNNBatch(5, "manh", pool)
    //    case "Varios" => NoLearner()
  }
}
