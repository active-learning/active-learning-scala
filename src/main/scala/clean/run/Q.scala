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
import clean.{ArgParser, Ds, Exp}
import ml.Pattern
import ml.classifiers._
import weka.filters.Filter

object Q extends Exp with ArgParser {
  val arguments = superArguments
  val context = "Qapp"
  init()

  def strats(pool: Seq[Pattern], seed: Int) = List(RandomSampling(pool))

  def isAlreadyDone(ds: Ds) = ds.isQCalculated

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    //queries
    ds.log("queries")
    val queries = if (ds.areQueriesFinished(pool, strat, run, fold)) {
      println(s"Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
      ds.queries(strat, run, fold, binaf, zscof)
    } else ds.writeQueries(pool, strat, run, fold, Int.MaxValue)

    //hits
    ds.log("hits")
    val learners = Seq(NB(), KNNBatch(5, "eucl", pool, weighted = true), C45())
    learners foreach { learner =>
      if (ds.areHitsFinished(pool, strat, learner, run, fold)) println(s"Hits already done for ${strat.abr}/$learner at pool $run.$fold.")
      ds.writeHits(pool, testSet, queries.toVector, strat, run, fold)(learner)
    }
  }

  def end(ds: Ds) {
    //Q
    val Q = ds.calculaQ(runs, folds)
    println(s"Q: $Q\n")
  }
}
