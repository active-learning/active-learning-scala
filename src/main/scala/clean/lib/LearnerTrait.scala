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

import ml.Pattern
import ml.classifiers._

trait LearnerTrait {
  def learners(learnersStr: Seq[String], pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = learnersStr map str2learner(pool, learnerSeed)

  def learnersfun(learnersStr: Seq[String], pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = learnersStr map str2learnerfun

  def learnersPool(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = List[Learner](
    //individuais
    KNNBatcha(5, "eucl", pool, weighted = true)
    , KNNBatcha(5, "manh", pool, weighted = true)
    , KNNBatcha(5, "eucl", pool, weighted = false)
    , KNNBatcha(5, "manh", pool, weighted = false)
    , C45()
    , NBBatch()
    , C452()
    //SVM usa filtro, então aparece mais abaixo, na outra função

    //Florestas, 3 tipos distintos de ensemble
    , RF(learnerSeed)
    , ABoo(learnerSeed)
    //RoF usa filtro, então aparece mais abaixo, na outra função

    //Baggings e 10NN
    //fora    , Knn10(pool, weighted = true)
    , BagC45(learnerSeed)
    , BagNB(learnerSeed)
    //    , Knn10(pool, weighted = false, distance_name = "eucl")
    //    , Knn10(pool, weighted = false, distance_name = "manh")
  )

  def learnersFpool(learnerSeed: Int = -1) = List[Learner](
    RoF(learnerSeed)
    , SVMLibRBF(learnerSeed)
  )

  def str2learner(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1)(str: String) = str match {
    case "bagnb" => BagNB(learnerSeed)
    case "bagc45" => BagC45(learnerSeed)
    case "10nnw" => Knn10(pool, weighted = true, distance_name = "eucl")
    case "10nn" => Knn10(pool, weighted = false, distance_name = "eucl")
    case "10nnmw" => Knn10(pool, weighted = true, distance_name = "manh")
    case "10nnm" => Knn10(pool, weighted = false, distance_name = "manh")

    case "aboo" => ABoo(learnerSeed)
    case "rof" => RoF(learnerSeed)
    case "nbb" => NBBatch()
    case "5nnw" => KNNBatcha(5, "eucl", pool, weighted = true)
    case "5nnmw" => KNNBatcha(5, "manh", pool, weighted = true)
    case "5nn" => KNNBatcha(5, "eucl", pool, weighted = false)
    case "5nnm" => KNNBatcha(5, "manh", pool, weighted = false)
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

  def str2learnerfun(str: String) = str match {
    case "bagnb" => (pool: Seq[Pattern], learnerSeed: Int) => BagNB(learnerSeed)
    case "bagc45" => (pool: Seq[Pattern], learnerSeed: Int) => BagC45(learnerSeed)
    case "10nnw" => (pool: Seq[Pattern], learnerSeed: Int) => Knn10(pool, weighted = true, distance_name = "eucl")
    case "10nn" => (pool: Seq[Pattern], learnerSeed: Int) => Knn10(pool, weighted = false, distance_name = "eucl")
    case "10nnmw" => (pool: Seq[Pattern], learnerSeed: Int) => Knn10(pool, weighted = true, distance_name = "manh")
    case "10nnm" => (pool: Seq[Pattern], learnerSeed: Int) => Knn10(pool, weighted = false, distance_name = "manh")
    case "aboo" => (pool: Seq[Pattern], learnerSeed: Int) => ABoo(learnerSeed)
    case "rof" => (pool: Seq[Pattern], learnerSeed: Int) => RoF(learnerSeed)
    case "nbb" => (pool: Seq[Pattern], learnerSeed: Int) => NBBatch()
    case "5nnw" => (pool: Seq[Pattern], learnerSeed: Int) => KNNBatcha(5, "eucl", pool, weighted = true)
    case "5nnmw" => (pool: Seq[Pattern], learnerSeed: Int) => KNNBatcha(5, "manh", pool, weighted = true)
    case "5nn" => (pool: Seq[Pattern], learnerSeed: Int) => KNNBatcha(5, "eucl", pool, weighted = false)
    case "5nnm" => (pool: Seq[Pattern], learnerSeed: Int) => KNNBatcha(5, "manh", pool, weighted = false)
    case "c45" => (pool: Seq[Pattern], learnerSeed: Int) => C45()
    case "c452" => (pool: Seq[Pattern], learnerSeed: Int) => C452()
    case "rf" => (pool: Seq[Pattern], learnerSeed: Int) => RF(learnerSeed)
    case "vfdt" => (pool: Seq[Pattern], learnerSeed: Int) => VFDT()
    case "poly1" => (pool: Seq[Pattern], learnerSeed: Int) => SVMLibDegree1(learnerSeed)
    case "rbf" => (pool: Seq[Pattern], learnerSeed: Int) => SVMLibRBF(learnerSeed)
  }
}
