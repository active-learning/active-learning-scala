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

import al.strategies.Strategy
import ml.Pattern
import weka.filters.Filter

trait nonQ extends Exp with LearnerTrait {
  val arguments = superArguments ++ List("learner:nb|5nn|c45|vfdt|ci|eci|i|ei|in|svm")

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    //queries (só no learner da strat: NoLearner pra Clu, 'fornecido' pra Gnos)
    ds.log("queries")
    val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold)) {
      println(s"Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
      ds.queries(strat, run, fold, binaf, zscof)
    } else ds.writeQueries(strat, run, fold, ds.Q)

    //hits (pra learner fornecido)
    ds.log("hits")
    if (ds.areHitsFinished(pool.size, strat, fixedLearner(pool, learnerSeed), run, fold)) println(s"Hits already done for ${strat.abr}/${fixedLearner(pool, learnerSeed)} at pool $run.$fold.")
    else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold)(fixedLearner(pool, learnerSeed))
  }

  def datasetClosing(ds: Ds) {
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = {
    val poolSize = ds.expectedPoolSizes(folds)
    val checks = for {
      s <- strats(Seq(), -1).toStream //.par
      r <- (0 until runs).toStream //.par
      f <- (0 until folds).toStream //.par
    } yield {
      lazy val res = ds.areQueriesFinished(poolSize(f), s, r, f) && ds.areHitsFinished(poolSize(f), s, fixedLearner(Seq(), -1), r, f)
      res
    }
    //    log(checks.mkString("\n"))
    checks forall (_ == true)
  }

  def datasetFinished() {
  }


  def end(): Unit = {
  }

}
