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

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], run: Int, fold: Int) = {
    //queries
    ds.writeQueries(pool, strat, run, fold, Int.MaxValue)

    ds.write("DROP TABLE h")
    ds.write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) )")
    //hits
    //    ???
    //    val learners = Seq(NB(), KNNBatch(5, "eucl", pool, weighted = true), C45())
    //    learners foreach ds.writeHits(pool, strat.queries, strat, run, fold)
  }

  def end(ds: Ds) {
    //    ???
    //    //Faz lista com 25 pseudoQs (um para cada pool); é o primeiro ponto de acc max do melhor dentre os 3 classificadores.
    //    val QNB_Q5NN_QC45 = (for {
    //      r <- (0 until runs).par
    //      f <- (0 until folds).par
    //      sql = s"select position from hit where run=$r and fold=$f and strategyid=1 and learnerid in (16,17,5) and pred=expe group by position,learnerid order by sum(value) desc, position asc limit 1"
    //    } yield {
    //      exec(sql).get.head.head
    //    }).toList
    //
    //    //Pega mediana.
    //    val QAccMax = QNB_Q5NN_QC45.sorted.toList(runs * folds / 2).toInt
    //    println(s"Q=$QAccMax")
    //
    //    exec(s"insert into res values (1,-1,-1,-1,-1,$QAccMax)") //todo: aqui quebra caso db esteja aberto como readOnly
  }
}
