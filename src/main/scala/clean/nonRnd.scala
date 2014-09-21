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

import al.strategies.{ClusterBased, ExpErrorReductionMargin, Strategy}
import ml.Pattern
import ml.classifiers._
import weka.filters.Filter

trait nonRnd extends Exp {
  val arguments = List("datasets-path", "file-with-dataset-names", "parallelize(runs folds):n|r|f|rf", "learner:nb|5nn|c45|vfdt|ci|eci|i|ei|in|svm")
  lazy val parallelRuns = args(2).contains("r")
  lazy val parallelFolds = args(2).contains("f")
  val samplingSize = 500
  init()

  def op(strat: Strategy, ds: Ds, pool: => Seq[Pattern], learnerSeed: Int, testSet: => Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    //queries (só no learner da strat: NoLearner pra Clu, 'fornecido' pra Gnos)
    ds.log("queries")
    val queries = if (ds.areQueriesFinished(pool, strat, run, fold)) {
      println(s"Queries already done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
      ds.queries(strat, run, fold, binaf, zscof)
    } else ds.writeQueries(pool, strat, run, fold, ds.Q)

    //hits (pra learner fornecido)
    ds.log("hits")
    ds.writeHits(pool, testSet, queries.toVector, strat, run, fold)(learner(pool, learnerSeed))
  }

  def end(ds: Ds) {
    ds.log("fim")
  }

  def isAlreadyDone(ds: Ds) = false
}
