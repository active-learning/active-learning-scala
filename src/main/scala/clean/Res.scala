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
import weka.filters.Filter

import scala.collection.mutable

trait Res extends Exp with Blob with Lock with LearnerTrait {
  val arguments = superArguments ++ List("learner:nb|5nn|c45|vfdt|ci|eci|i|ei|in|svm")
  val measures = mutable.Queue[Double]()

  def calculate(cms: List[Array[Array[Int]]]): Double

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    if (!ds.isQCalculated) log(s"Q was not found for ${strat.abr}/${strat.learner} at pool $run.$fold!", 2)
    else if (!ds.areQueriesFinished(pool.size, strat, run, fold)) log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 2)
    else if (!ds.areHitsFinished(pool.size, strat, learner(pool, learnerSeed), run, fold)) log(s"Conf. matrices were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 2)
    else {
      val cms = ds.getCMs(strat, learner(pool, learnerSeed), run, fold)
      acquire()
      measures.enqueue(calculate(cms))
      release()
    }
  }

  def end(ds: Ds) {
    ???
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = {
    ???
    val poolSize = ds.expectedPoolSizes(folds)
    val checks = for {
      s <- strats(Seq(), -1).toStream //.par
      r <- (0 until runs).toStream //.par
      f <- (0 until folds).toStream //.par
    } yield {
      lazy val res = ds.areHitsFinished(poolSize(f), s, learner(Seq(), -1), r, f)
      res
    }
    checks forall (_ == true)
  }

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
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "entropy", samplingSize),
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "gmeans+residual", samplingSize),
    ExpErrorReductionMargin(learner(pool, learnerSeed), pool, "accuracy", samplingSize),
    new SGmulti(learner(pool, learnerSeed), pool, "consensus"),
    new SGmulti(learner(pool, learnerSeed), pool, "majority"),
    new SGmultiJS(learner(pool, learnerSeed), pool)
    //    ,
    //      SVMmulti(pool, "SELF_CONF"),
    //    SVMmulti(pool, "KFF"),
    //    SVMmulti(pool, "BALANCED_EE"),
    //    SVMmulti(pool, "SIMPLE")
  )
}
