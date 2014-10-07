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

import al.strategies.{SVMmulti, StrategyAgnostic, Strategy}
import ml.Pattern
import weka.filters.Filter

object all extends Exp with LearnerTrait with StratsTrait {
  val context = "allApp"
  val arguments = superArguments
  val ignoreNotDone = false
  run()

  def strats(pool: Seq[Pattern], learnerSeed: Int) = agnosticAndSVMStrats(pool) ++ (allLearners(pool, learnerSeed) flatMap { learner => learnerDependentStrats(pool, learner)})

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    if (!ds.isQCalculated) error(s"Q is not calculated!")
    else {
      //Assumes Q is calculated == rndnb/5nn/c45 qs are ready.
      val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold)) {
        println(s"Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
        ds.queries(strat, run, fold, binaf, zscof)
      } else ds.writeQueries(strat, run, fold, ds.Q)

      strat match {
        case st: StrategyAgnostic =>
          allLearners(pool, learnerSeed) foreach { learner =>
            ds.log(s"Agn hits [$st $learner] at pool $run.$fold.")
            if (ds.areHitsFinished(pool.size, strat, learner, run, fold)) println(s"Hits already done for ${strat.abr}/${learner} at pool $run.$fold.")
            else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(learner)
          }
        case st =>
          //hits (pra learner fornecido)
          ds.log(s"gn hits [$st ${st.learner}] at pool $run.$fold.")
          if (ds.areHitsFinished(pool.size, strat, strat.learner, run, fold)) println(s"Hits already done for ${strat.abr}/${strat.learner} at pool $run.$fold.")
          else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(strat.learner)
      }
    }
  }

  def datasetFinished(ds: Ds) {
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = {
    val poolSize = ds.expectedPoolSizes(folds)
    val checks = for {
      s <- strats(Seq(), -1).toStream //.par
      l <- allLearners().toStream //.par
      r <- (0 until runs).toStream //.par
      f <- (0 until folds).toStream //.par
    } yield {
      lazy val res = ds.areQueriesFinished(poolSize(f), s, r, f) && ds.areHitsFinished(poolSize(f), s, l, r, f)
      res
    }
    checks forall (_ == true)
  }

  def end(res: Map[String, Boolean]): Unit = {
  }
}
