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

import al.strategies.{RandomSampling, Strategy}
import clean._
import clean.res._
import ml.Pattern
import ml.classifiers.Learner
import weka.filters.Filter

import scala.collection.mutable

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with MeasuresTrait {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   val sqls = mutable.Queue[String]()
   run()

   def calculate(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int, meas: Measure) = meas.calc(ds, cms, tsSize)

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      allMeasures(maxQueries(ds)) foreach { meas => if (!sqls.contains("cancel")) {
         if (meas.id(ds) == 15 || meas.id(ds) == 16) {
            specialLearners(Seq()) foreach storeSQL(pool.size, ds, RandomSampling(Seq()), run, fold, testSet.size, meas)
         } else {
            stratsemLearnerExterno() foreach { strat =>
               if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
                  ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
                  acquire()
                  sqls += "cancel"
                  release()
               } else if (strat.id >= 17 && strat.id <= 21 || strat.id == 969) storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)(strat.learner)
               else allLearners() foreach storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)
            }
            allLearners() foreach { learner =>
               stratcomLearnerExterno(learner) foreach { strat =>
                  if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
                     ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
                     acquire()
                     sqls += "cancel"
                     release()
                  } else storeSQL(pool.size, ds, strat, run, fold, testSet.size, meas)(learner)
               }
            }
         }
      }
      }
   }

   def storeSQL(poolSize: Int, ds: Ds, strat: Strategy, run: Int, fold: Int, testSetSize: Int, meas: Measure)(learner: Learner) =
      if (!sqls.contains("cancel")) {
      ds.log(s"$strat $learner $run $fold")
      if (!ds.areHitsFinished(poolSize, Seq(), strat, learner, run, fold, null, null, completeIt = false, maxQueries(ds) - ds.nclasses + 1)) {
         ds.log(s"Conf. matrices were not finished for ${strat.abr}/$learner/svm? at pool $run.$fold!")
         acquire()
         sqls += "cancel"
         release()
      } else ds.getMeasure(meas, strat, learner, run, fold) match {
         case Some(_) => ds.log(s"Measure $meas already calculated for ${strat.abr}/${strat.learner} at pool $run.$fold!")
         case None =>
            val cms = ds.getCMs(strat, learner, run, fold, maxQueries(ds) - ds.nclasses + 1) //pega todas CMs
            if (cms.size < maxQueries(ds) - ds.nclasses + 1) ds.quit(s"Couldn't take at least ${maxQueries(ds) - ds.nclasses + 1} queries, ${cms.size} only.")
            val tsSize = contaTotal(cms.head._2)
            if (testSetSize != tsSize) ds.quit("Hits differs from testSetSize!")
            val v = calculate(ds, cms, tsSize, meas)
            acquire()
            sqls += ds.measureToSQL(meas, v, strat.id, learner, run, fold)
            release()
      }
   }

   def datasetFinished(ds: Ds) {
      if (!ds.isClosed) {
         if (sqls.contains("cancel")) ds.log("Refused to measure on incomplete dataset results. Look at log to see why it could not be completed now, probably results have not even started.", 20)
         else if (sqls.nonEmpty) {
            ds.batchWrite(sqls.toList)
            ds.log("Possible measures completed.")
         }
         ds.log("fim deste")
      }
      sqls.clear()
   }

   def isAlreadyDone(ds: Ds) = false //ds.isMeasureComplete(measure, s.id, learner.id)

   def end(res: Map[String, Boolean]): Unit = {
      println(s"prontos. \n${args.toList}")
   }
}
