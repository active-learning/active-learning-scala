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

import al.strategies.{ClusterBased, RandomSampling, Strategy}
import ml.Pattern
import ml.classifiers._

object Q extends Exp {
  val arguments = List("datasets-path", "file-with-dataset-names", "paralleliz(runs folds):r|f|rf")
  lazy val parallelRuns = args(2).contains("r")
  lazy val parallelFolds = args(2).contains("f")
  init()

  def strats(pool: Seq[Pattern]) = List(RandomSampling(pool), ClusterBased(pool))

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], run: Int, fold: Int) = {
    //queries
    ds.writeQueries(pool, strat, run, fold, Int.MaxValue)

    if (ds.read("SELECT count(1) from h").size == 0) {
      //ajeita tabela
      ds.write("DROP TABLE h")
      ds.write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) )")
    }

    //hits
    val queries = ds.queries(strat, run, fold)
    val learners = Seq(NB(), KNNBatch(5, "eucl", pool, weighted = true), C45())
    learners foreach ds.writeHits(pool, testSet, queries, strat, run, fold)
  }

  def end(ds: Ds) {
    //Q
    val Q = ds.Q.getOrElse(ds.calculaQ)
    println(s"Q: $Q")
  }
}
