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
//package clean.tex
//
//import al.strategies._
//import clean._
//import ml.Pattern
//import ml.classifiers.SVMLib
//import weka.filters.Filter
//
///**
// * Lists in a tex table datasets which have the measure already calculated for all given learners.
// */
//object datasetstab extends Exp with Blob with Lock with LearnerTrait with CM {
//  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "medida:alca|alcg")
//  val context = "datasetstabtex"
//  val ignoreNotDone = true
//  val feitos = run()
//
//  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
//  }
//
//  def datasetFinished(ds: Ds) {
//  }
//
//  def isAlreadyDone(ds: Ds) = if (!ds.isQCalculated) false
//  else {
//    val poolSize = ds.expectedPoolSizes(folds)
//    val checks = for {
//      s <- strats(Seq(), -1).toStream //.par
//      l <- learners(learnersStr).toStream
//      r <- (0 until runs).toStream //.par
//      f <- (0 until folds).toStream //.par
//    } yield {
//      val learner = if (s.id >= 17 && s.id <= 20) SVMLib() else l
//      lazy val res = {
//        ds.areQueriesFinished(poolSize(f), s, r, f) && ds.areHitsFinished(poolSize(f), s, learner, r, f) && (ds.getMeasure(measure, s, learner, r, f) match {
//          case Some(x) => true
//          case None => false
//        })
//      }
//      res
//    }
//    checks forall (_ == true)
//  }
//
//  def end(res: Map[String, Boolean]): Unit = {
//    res.filter(_._2 == true).map(_._1) foreach println
//  }
//
//  def strats(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = List(
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
//    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "entropy"),
//    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "gmeans+residual"),
//    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "accuracy"),
//    new SGmulti(fixedLearner(pool, learnerSeed), pool, "consensus"),
//    new SGmulti(fixedLearner(pool, learnerSeed), pool, "majority"),
//    new SGmultiJS(fixedLearner(pool, learnerSeed), pool),
//    SVMmulti(pool, "SELF_CONF"),
//    SVMmulti(pool, "KFF"),
//    SVMmulti(pool, "BALANCED_EE"),
//    SVMmulti(pool, "SIMPLE")
//  )
//
//}
//
//
///*
//exemplo de eu repetindo software:
//  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
//    if (!ds.isQCalculated) log(s"Q was not found for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
//    else if (!ds.areQueriesFinished(pool.size, strat, run, fold)) log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
//    else if (!ds.areHitsFinished(pool.size, strat, learner(pool, learnerSeed), run, fold)) log(s"Conf. matrices were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
//    else if (ds.getMeasure(measure, strat, learner(pool, 42), run, fold).isEmpty) {
//      val cms = ds.getCMs(strat, learner(pool, learnerSeed), run, fold)
//
//  def isAlreadyDone(ds: Ds) = {
//    val checks = for {
//      s <- strats(Seq(), -1).toStream //.par
//      r <- (0 until runs).toStream //.par
//      f <- (0 until folds).toStream //.par
//    } yield {
//      lazy val res = ds.getMeasure(measure, s, learner(Seq(), -1), r, f) match {
//        case Some(_) => true
//        case None => false
// */