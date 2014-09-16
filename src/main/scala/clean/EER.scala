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

import al.strategies.{ExpErrorReductionMargin, Strategy}
import ml.Pattern
import ml.classifiers._

object EER extends Exp {
  val arguments = List("datasets-path", "file-with-dataset-names", "paralleliz(runs folds):n|r|f|rf", "learner:nb|5nn|c45|vfdt|ci|eci|i|ei|in|svm")
  lazy val parallelRuns = args(2).contains("r")
  lazy val parallelFolds = args(2).contains("f")
  val samplingSize = 500

  init()

  def strats(pool: Seq[Pattern], learnerSeed: Int) = List(
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "entropy", samplingSize),
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "gmeans+residual", samplingSize),
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "accuracy", samplingSize)
  )

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int) = {
    //queries
    ds.writeQueries(pool, strat, run, fold, Int.MaxValue)

    //hits
    val queries = ds.queries(strat, run, fold)
    ds.writeHits(pool, testSet, queries, strat, run, fold)(learner(pool, learnerSeed))
  }

  def end(ds: Ds) {
    log("fim")(ds.toString)
  }

  def learner(pool: Seq[Pattern], learnerSeed: Int) = args(3) match {
    case "nb" => NB()
    case "5nn" => KNNBatch(5, "eucl", pool, weighted = true)
    case "c45" => C45()
    case "vfdt" => VFDT()
    case "CI" => CIELM(learnerSeed)
    case "ECI" => ECIELM(learnerSeed)
    case "I" => IELM(learnerSeed)
    case "EI" => EIELM(learnerSeed)
    case "intera" => interaELM(learnerSeed)
    case "SVM" => SVMLib(learnerSeed)

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