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
import clean.{Ds, Exp, LearnerTrait, StratsTrait}
import ml.Pattern
import ml.classifiers.SVMLib
import weka.filters.Filter

object all extends Exp with LearnerTrait with StratsTrait {
  val context = "allApp"
  val arguments = superArguments
  val ignoreNotDone = false
  run()

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    if (!ds.isQCalculated) error(s"Q is not calculated!")
    else {
      //rnd clu svm / lff lfd
      stratsFilterFreeSemLearnerExterno(pool).zip(stratsFilterFreeSemLearnerExterno(fpool)) foreach { case (strat, fstrat) =>
        ds.log(s"$strat ...")
        //queries
        val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold)) {
          println(s"agn and SVM Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
          ds.queries(strat, run, fold, null, null)
        } else ds.writeQueries(strat, run, fold, ds.Q)
        val fqueries = ds.queries(fstrat, run, fold, binaf, zscof)
        //hits
        if (strat.id >= 17 && strat.id <= 20) {
          val learner = SVMLib()
          ds.log(s"SVM hits [$strat $learner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(pool.size, strat, learner, run, fold)) println(s"Hits already done for ${strat.abr}/$learner at pool $run.$fold.")
          else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
        } else {
          learnersFilterFree(pool, learnerSeed) foreach { learner =>
            ds.log(s"Agn hits [$strat $learner] at pool $run.$fold.", 20)
            if (ds.areHitsFinished(pool.size, strat, learner, run, fold)) println(s"Hits already done for ${strat.abr}/$learner at pool $run.$fold.")
            else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
          }
          learnersFilterDependent(learnerSeed) foreach { flearner =>
            ds.log(s"Agnf hits [$fstrat $flearner] at pool $run.$fold.", 20)
            if (ds.areHitsFinished(fpool.size, fstrat, flearner, run, fold)) println(s"Hits already done for ${fstrat.abr}/$flearner at pool $run.$fold.")
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
          val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold)) {
            println(s"nonf Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(strat, run, fold, null, null)
          } else ds.writeQueries(strat, run, fold, ds.Q)
          //hits
          ds.log(s"nonFilter hits [$strat $learner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(pool.size, strat, learner, run, fold)) println(s"Hits already done for ${strat.abr}/$learner at pool $run.$fold.")
          else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
          ds.log(s"$strat ok.")
        }
      }

      //restoSemF / lfd
      learnersFilterDependent(learnerSeed) foreach { flearner =>
        stratsFilterFreeComLearnerExterno(fpool, flearner) foreach { case fstrat =>
          ds.log(s"$fstrat ...")
          //queries
          val fqueries = if (ds.areQueriesFinished(pool.size, fstrat, run, fold)) {
            println(s"fQueries already done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(fstrat, run, fold, binaf, zscof)
          } else ds.writeQueries(fstrat, run, fold, ds.Q)

          //hits
          ds.log(s"Filter hits [$fstrat $flearner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(fpool.size, fstrat, flearner, run, fold)) println(s"fHits already done for ${fstrat.abr}/$flearner at pool $run.$fold.")
          else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold)(flearner)
          ds.log(s"$fstrat ok.")
        }
      }

      //restoComF / lff lfd
      (learnersFilterFree(fpool, learnerSeed) ++ learnersFilterDependent(learnerSeed)) foreach { flearner =>
        stratsFilterDependentComLearnerExterno(fpool, flearner) foreach { case fstrat =>
          ds.log(s"$fstrat ...")
          //queries
          val fqueries = if (ds.areQueriesFinished(pool.size, fstrat, run, fold)) {
            println(s"fQueries maha already done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(fstrat, run, fold, binaf, zscof)
          } else ds.writeQueries(fstrat, run, fold, ds.Q)

          //hits
          ds.log(s"Filter maha hits [$fstrat $flearner] at pool $run.$fold.", 20)
          if (ds.areHitsFinished(fpool.size, fstrat, flearner, run, fold)) println(s"fHits already done for ${fstrat.abr}/$flearner at pool $run.$fold.")
          else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold)(flearner)
          ds.log(s"$fstrat ok.")
        }
      }
    }
  }

  def datasetFinished(ds: Ds) {
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = {
    //    val poolSize = ds.expectedPoolSizes(folds)
    //    val checks = strats(Seq(), -1).toStream flatMap {
    //      case s@(_: StrategyAgnostic) =>
    //        for {
    //          r <- (0 until runs).toStream
    //          f <- (0 until folds).toStream
    //        } yield {
    //          lazy val res0 = ds.areQueriesFinished(poolSize(f), s, r, f) && learnersF().toStream.forall { l =>
    //            lazy val res = ds.areHitsFinished(poolSize(f), s, l, r, f)
    //            res
    //          }
    //          res0
    //        }
    //      case s =>
    //        for {
    //          r <- (0 until runs).toStream
    //          f <- (0 until folds).toStream
    //        } yield {
    //          lazy val res = ds.areQueriesFinished(poolSize(f), s, r, f) && ds.areHitsFinished(poolSize(f), s, s.learner, r, f)
    //          res
    //        }
    //    }
    //    checks forall (_ == true)
    false
  }

  def end(res: Map[String, Boolean]): Unit = {
  }
}
