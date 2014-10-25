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

import al.strategies.{Strategy, RandomSampling}
import clean._
import clean.res.Measure
import ml.Pattern
import ml.classifiers.Learner
import weka.filters.Filter

import scala.collection.mutable

object all extends Exp with LearnerTrait with StratsTrait with Lock with CM with MeasuresTrait {
  val context = "allApp"
  val arguments = superArguments
  val ignoreNotDone = false
  run()

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    //rnd clu svm maj / lff lfd
    stratsFilterFreeSemLearnerExterno(pool).zip(stratsFilterFreeSemLearnerExterno(fpool)) foreach { case (strat, fstrat) =>
      ds.log(s"$strat ...")
      //queries
      val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true)) {
        println(s"agn, SVM and maj Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
        ds.queries(strat, run, fold, null, null)
      } else ds.writeQueries(strat, run, fold, ds.Q)
      val fqueries = ds.queries(fstrat, run, fold, binaf, zscof)
      //hits
      if (strat.id >= 17 && strat.id <= 21) {
        val learner = strat.learner
        ds.log(s"SVM/Maj hits [$strat $learner] at pool $run.$fold.", 20)
        if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true)) println(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
        else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
      } else {
        learnersFilterFree(pool, learnerSeed) foreach { learner =>
          ds.log(s"Agn hits [$strat $learner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true)) println(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
          else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
        }
        learnersFilterDependent(learnerSeed) foreach { flearner =>
          ds.log(s"Agnf hits [$fstrat $flearner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(fpool.size, testSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true)) println(s"Hits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
          else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold)(flearner)
        }
      }
      ds.log(s"$strat ok.")
    }

    //restoSemF / lff
    learnersFilterFree(pool, learnerSeed) foreach { learner =>
      stratsFilterFreeComLearnerExterno(pool, learner) foreach { case strat =>
        ds.log(s"$strat ...")
        //queries
        val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true)) {
          println(s"nonf Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
          ds.queries(strat, run, fold, null, null)
        } else ds.writeQueries(strat, run, fold, ds.Q)
        //hits
        ds.log(s"nonFilter hits [$strat $learner] at pool $run.$fold.", 20)
        if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true)) println(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
        else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
        ds.log(s"$strat ok.")
      }
    }

    //restoSemF / lfd
    learnersFilterDependent(learnerSeed) foreach { flearner =>
      stratsFilterFreeComLearnerExterno(fpool, flearner) foreach { case fstrat =>
        ds.log(s"$fstrat ...")
        //queries
        val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true)) {
          println(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
          ds.queries(fstrat, run, fold, binaf, zscof)
        } else ds.writeQueries(fstrat, run, fold, ds.Q)

        //hits
        ds.log(s"Filter hits [$fstrat $flearner] at pool $run.$fold.", 20)
        if (ds.areHitsFinished(fpool.size, testSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true)) println(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
        else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold)(flearner)
        ds.log(s"$fstrat ok.")
      }
    }

    //restoComF / lff lfd
    (learnersFilterFree(fpool, learnerSeed) ++ learnersFilterDependent(learnerSeed)) foreach { flearner =>
      stratsFilterDependentComLearnerExterno(fpool, flearner) foreach { case fstrat =>
        ds.log(s"$fstrat ...")
        //queries
        val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true)) {
          println(s"fQueries maha  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
          ds.queries(fstrat, run, fold, binaf, zscof)
        } else ds.writeQueries(fstrat, run, fold, ds.Q)

        //hits
        ds.log(s"Filter maha hits [$fstrat $flearner] at pool $run.$fold.", 20)
        if (ds.areHitsFinished(fpool.size, testSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true)) println(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
        else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold)(flearner)
        ds.log(s"$fstrat ok.")
      }
    }

    //medidas
    if (!ds.isQCalculated) ds.error(s"Q is not calculated!")
    else allMeasures foreach { meas =>
      if (meas.id == 15 || meas.id == 16) {
        specialLearners(Seq()) foreach storeSQL(pool.size, ds, RandomSampling(Seq()), run, fold, testSet.size, meas)
      } else {
        stratsemLearnerExterno() foreach { strat =>
          if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false)) ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
          else if (strat.id >= 17 && strat.id <= 21) storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)(strat.learner)
          else allLearners() foreach storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)
        }
        allLearners() foreach { learner =>
          stratcomLearnerExterno(learner) foreach { strat =>
            if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false)) {
              ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
              //              sqls += "cancel"
            } else storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)(learner)
          }
        }
      }
    }
  }

  def calculate(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int, meas: Measure) = meas.calc(ds, cms, tsSize)

  def storeSQL(poolSize: Int, ds: Ds, strat: Strategy, run: Int, fold: Int, testSetSize: Int, meas: Measure)(learner: Learner): Unit = {
    //    log(s"$strat $learner $run $fold")
    //    if (!ds.areHitsFinished(poolSize, Seq(), strat, learner, run, fold, null, null, completeIt = false)) ds.log(s"Conf. matrices were not finished for ${strat.abr}/$learner/svm? at pool $run.$fold!")
    //    else ds.getMeasure(meas, strat, learner, run, fold) match {
    //      case Some(_) => log(s"Measure $meas already calculated for ${strat.abr}/${strat.learner} at pool $run.$fold!")
    //      case None =>
    //        val cms = ds.getCMs(strat, learner, run, fold)
    //        if (cms.size < ds.Q - ds.nclasses + 1) ds.quit(s"Couldn't take at least ${ds.Q - ds.nclasses + 1} queries, ${cms.size} only.")
    //        val tsSize = contaTotal(cms.head._2)
    //        if (testSetSize != tsSize) ds.quit("Hits differs from testSetSize!")
    //        val v = calculate(ds, cms, tsSize, meas)
    //        acquire()
    //        ds.putMeasureValue(meas, v, strat, learner, run, fold)
    //        release()
    //    }
  }

  def datasetFinished(ds: Ds) {
  }

  def isAlreadyDone(ds: Ds) = false

  def end(res: Map[String, Boolean]): Unit = {
  }
}
