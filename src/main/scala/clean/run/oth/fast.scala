///*
//
//active-learning-scala: Active Learning library for Scala
//Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
//*/
//
//package clean.run.oth
//
//import al.strategies._
//import clean.nonQ
//import ml.Pattern
//
//object fast extends nonQ {
//  val context = "fastApp"
//  run()
//
//  def strats(pool: Seq[Pattern], learnerSeed: Int) = List(
//    RandomSampling(pool),
//    ClusterBased(pool),
//    Uncertainty(fixedLearner(pool, learnerSeed), pool),
//    Entropy(fixedLearner(pool, learnerSeed), pool),
//    Margin(fixedLearner(pool, learnerSeed), pool),
//    DensityWeighted(fixedLearner(pool, learnerSeed), pool, 1, "eucl"),
//    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "cheb"),
//    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "eucl"),
//    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "maha"),
//    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "manh"),
//    MahalaWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, 1, 1),
//    new SGmulti(fixedLearner(pool, learnerSeed), pool, "consensus"),
//    new SGmulti(fixedLearner(pool, learnerSeed), pool, "majority"),
//    new SGmultiJS(fixedLearner(pool, learnerSeed), pool)
//  )
//}
