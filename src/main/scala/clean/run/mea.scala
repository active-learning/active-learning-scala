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

package clean.run

import al.strategies.{Strategy, StrategyAgnostic}
import clean._
import ml.Pattern
import ml.classifiers.{Learner, SVMLib}
import weka.filters.Filter

import scala.collection.mutable

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM {
  val context = "meaApp"
  val arguments = superArguments ++ List("medida:alca|alcg")
  val ignoreNotDone = false
  val sqls = mutable.Queue[String]()
  run()

  def calculate(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = measure.calc(ds, cms, tsSize)

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    if (!ds.isQCalculated) ds.error(s"Q is not calculated!")
    else {
      stratsemLearnerExterno() foreach { strat =>
        if (!ds.areQueriesFinished(pool.size, strat, run, fold)) ds.quit(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
        else if (strat.id >= 17 && strat.id <= 20) storeSQL(pool.size, ds, strat, run, fold)(SVMLib())
        else allLearners() foreach storeSQL(pool.size, ds, strat, run, fold)
      }
      allLearners() foreach { learner =>
        stratcomLearnerExterno(learner) foreach { strat =>
          if (!ds.areQueriesFinished(pool.size, strat, run, fold)) ds.quit(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
          else storeSQL(pool.size, ds, strat, run, fold)(learner)
        }
      }
    }
  }

  def storeSQL(poolSize: Int, ds: Ds, strat: Strategy, run: Int, fold: Int)(learner: Learner): Unit = {
    log(s"$strat $learner $run $fold")
    if (!ds.areHitsFinished(poolSize, strat, learner, run, fold)) ds.quit(s"Conf. matrices were not finished for ${strat.abr}/$learner/svm? at pool $run.$fold!")
    else ds.getMeasure(measure, strat, learner, run, fold) match {
      case Some(_) => log(s"Measure $measure already calculated for ${strat.abr}/${strat.learner} at pool $run.$fold!")
      case None =>
        val cms = ds.getCMs(strat, learner, run, fold).take(ds.Q - ds.nclasses + 1)
        if (cms.size != ds.Q - ds.nclasses + 1) ds.quit(s"Couldn't take ${ds.Q - ds.nclasses + 1} queries, ${cms.size} only.")
        val total = cms.values.foldLeft(0) { (sum, cm) =>
          sum + contaTotal(cm)
        }
        val tsSize = contaTotal(cms.head._2)
        if (total.toDouble / tsSize != cms.size) ds.error("problems!")
        val v = calculate(ds, cms, tsSize)
        acquire()
        sqls += ds.measureToSQL(measure, v, strat.id, learner, run, fold)
        release()
    }
  }

  def datasetFinished(ds: Ds) {
    ds.batchWrite(sqls.toList)
    ds.log("fim deste")
    sqls.clear()
  }

  def isAlreadyDone(ds: Ds) = {
    //    val checks = for {
    //      s <- strats(Seq(), -1).toStream //.par
    //      r <- (0 until runs).toStream //.par
    //      f <- (0 until folds).toStream //.par
    //    } yield {
    //      val learner = if (s.id >= 17 && s.id <= 20) SVMLib() else fixedLearner()
    //      lazy val res = ds.getMeasure(measure, s, learner, r, f) match {
    //        case Some(_) => true
    //        case None => false
    //      }
    //      res
    //    }
    //    checks forall (_ == true)
    false
  }

  def end(res: Map[String, Boolean]): Unit = {
  }
}
