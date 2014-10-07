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

package clean.res

import al.strategies._
import clean._
import ml.Pattern
import ml.classifiers.SVMLib
import weka.filters.Filter

import scala.collection.mutable

object res extends Exp with Blob with Lock with LearnerTrait with CM {
  lazy val arguments = superArguments ++ List("learner:nb|5nn|c45|vfdt|ci|eci|i|ei|in|svm", "medida:alca|alcg")
  val context = "res"
  val values = mutable.Map[(Int, Int, Int), Double]()
  val ignoreNotDone = false
  run()

  def calculate(cms: List[Array[Array[Int]]], total: Int) = measure.calc(cms, total)

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    val learner = if (strat.id >= 17 && strat.id <= 20) SVMLib() else fixedLearner()
    if (!ds.isQCalculated) log(s"Q was not found for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
    else if (!ds.areQueriesFinished(pool.size, strat, run, fold)) log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
    else if (!ds.areHitsFinished(pool.size, strat, learner, run, fold)) log(s"Conf. matrices were not finished for ${strat.abr}/${learner}/svm? at pool $run.$fold!", 20)
    else {
      ds.getMeasure(measure, strat, learner, run, fold) match {
        case Some(_) => log(s"Measure $measure already calculated for ${strat.abr}/${strat.learner} at pool $run.$fold!")
        case None =>
          val cms = ds.getCMs(strat, learner, run, fold).take(ds.Q)
          val total = cms.foldLeft(0) { (sum, cm) =>
            //        println(s"$sum ${contaTotal(cm)} ${testSet.size.toDouble}")
            sum + contaTotal(cm)
          }
          val qtdCMsEstimado = total / testSet.size.toDouble
          val expected = (strat.id, learner.id) match {
            case (sid, lid) if sid == 0 && lid < 4 => pool.size - ds.nclasses + 1
            case (sid, lid) if sid == 0 && lid > 3 || sid > 0 => ds.Q - ds.nclasses + 1
          }
          if (cms.size != qtdCMsEstimado)
            error(s"Total ${cms.size} de CMs difere de $qtdCMsEstimado estimado para ${strat.abr}/${learner} at pool $run.$fold!\n Q:${ds.Q} CMs:${cms.size} |cm|:${cms.head.size} |U|:${pool.size} |testset|:${testSet.size}")
          if (qtdCMsEstimado != expected)
            error(s"Total $qtdCMsEstimado de CMs difere de $expected esperado para ${strat.abr}/${learner} at pool $run.$fold!\n Q:${ds.Q} CMs:${cms.size} |cm|:${cms.head.size} |U|:${pool.size} |testset|:${testSet.size}")
          val v = calculate(cms, total)
          acquire()
          values += (strat.id, run, fold) -> v
          release()
      }
    }
  }

  def datasetFinished(ds: Ds) {
    if (values.size != strats(Seq(), -1).size * runs * folds) ds.error(s"Not all strategies were complete: ${values.size} values should be ${strats(Seq(), -1).size * runs * folds}!")
    else {
      val sqls = values map { case ((s, r, f), v) =>
        val learner = if (s >= 17 && s <= 20) SVMLib() else fixedLearner()
        ds.measureToSQL(measure, v, s, learner, r, f)
      }
      ds.batchWrite(sqls.toList)
      ds.log("fim deste")
    }
  }

  def isAlreadyDone(ds: Ds) = {
    val checks = for {
      s <- strats(Seq(), -1).toStream //.par
      r <- (0 until runs).toStream //.par
      f <- (0 until folds).toStream //.par
    } yield {
      val learner = if (s.id >= 17 && s.id <= 20) SVMLib() else fixedLearner()
      lazy val res = ds.getMeasure(measure, s, learner, r, f) match {
        case Some(_) => true
        case None => false
      }
      res
    }
    checks forall (_ == true)
  }

  def end(res: Map[String, Boolean]): Unit = {
  }

  def strats(pool: Seq[Pattern] = Seq(), learnerSeed: Int = -1) = List(
    RandomSampling(pool),
    ClusterBased(pool),
    Uncertainty(fixedLearner(pool, learnerSeed), pool),
    Entropy(fixedLearner(pool, learnerSeed), pool),
    Margin(fixedLearner(pool, learnerSeed), pool),
    DensityWeighted(fixedLearner(pool, learnerSeed), pool, 1, "eucl"),
    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "cheb"),
    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "eucl"),
    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "maha"),
    DensityWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, "manh"),
    MahalaWeightedTrainingUtility(fixedLearner(pool, learnerSeed), pool, 1, 1),
    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "entropy"),
    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "gmeans+residual"),
    ExpErrorReductionMargin(fixedLearner(pool, learnerSeed), pool, "accuracy"),
    new SGmulti(fixedLearner(pool, learnerSeed), pool, "consensus"),
    new SGmulti(fixedLearner(pool, learnerSeed), pool, "majority"),
    new SGmultiJS(fixedLearner(pool, learnerSeed), pool),
    SVMmulti(pool, "SELF_CONF"),
    SVMmulti(pool, "KFF"),
    SVMmulti(pool, "BALANCED_EE"),
    SVMmulti(pool, "SIMPLE")
  )
}


/*
exemplo de eu repetindo software:
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
import clean.res.Measure
import ml.Pattern
import weka.filters.Filter

import scala.collection.mutable

trait Res extends Exp with Blob with Lock with LearnerTrait with CM {
  val values = mutable.Map[(Int, Int, Int), Double]()
  val measure: Measure

  def calculate(cms: List[Array[Array[Int]]], total: Int): Double

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    if (!ds.isQCalculated) log(s"Q was not found for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
    else if (!ds.areQueriesFinished(pool.size, strat, run, fold)) log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
    else if (!ds.areHitsFinished(pool.size, strat, learner(pool, learnerSeed), run, fold)) log(s"Conf. matrices were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!", 20)
    else if (ds.getMeasure(measure, strat, learner(pool, 42), run, fold).isEmpty) {
      val cms = ds.getCMs(strat, learner(pool, learnerSeed), run, fold)
      val total = cms.foldLeft(0)((hits, cm) => contaTotal(cm))
      if (total != testSet.size) error(s"Total $total de hits na CM difere do tamanho ${ds.n} do cjt de teste!")

      val v = calculate(cms, total)
      ds.putMeasureValue(measure, v, strat, learner(pool, 42), run, fold)
      acquire()
      values += (strat.id, run, fold) -> v
      release()
    } else log(s"Measure already calculated for ${(measure, strat, learner(pool, 42), run, fold)}!")
  }

  def end(ds: Ds) {
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = {
    val checks = for {
      s <- strats(Seq(), -1).toStream //.par
      r <- (0 until runs).toStream //.par
      f <- (0 until folds).toStream //.par
    } yield {
      lazy val res = ds.getMeasure(measure, s, learner(Seq(), -1), r, f) match {
        case Some(_) => true
        case None => false
      }
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

 */