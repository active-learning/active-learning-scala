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
import util.Datasets

import scala.io.Source
import scala.util.Random

trait Exp extends AppWithUsage {
  lazy val path = args(0)
  lazy val datasets = Source.fromFile(args(1)).getLines().filter(_.length > 2).filter(!_.startsWith("#"))
  val parallelRuns: Boolean
  val parallelFolds: Boolean

  def strats(pool: Seq[Pattern]): List[Strategy]

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], run: Int, fold: Int)

  def end(ds: Ds)

  override def init() {
    super.init()
    datasets foreach { dataset =>
      val ds = Ds(path)(dataset)
      ds.open()
      log(s"Processing ${ds.n} instances ...")(ds.toString)
      (if (parallelRuns) (0 until Global.runs).par else 0 until Global.runs) foreach { run =>
        val shuffled = new Random(run).shuffle(ds.patterns)
        Datasets.kfoldCV(shuffled, k = Global.folds, parallelFolds) { (tr, ts, fold, minSize) =>
          log(s"Pool $run.$fold (${tr.size} instances) ...")(ds.toString)

          //Ordena pool e faz versão filtrada.
          val pool = new Random(fold).shuffle(tr.sortBy(_.id))
          val filteredPool = {
            val binaf = Datasets.binarizeFilter(tr)
            val binarizedTr = Datasets.applyFilter(binaf)(tr)
            val zscof = Datasets.zscoreFilter(binarizedTr)
            val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
            new Random(fold).shuffle(filteredTr.sortBy(_.id))
          }
          //        if (pool.zip(filteredPool).forall(x => x._1.id == x._2.id)) log("Ids foram mantidos após filtro.")
          //        else throw new Error("Ids inconsistentes!")

          strats(pool) foreach { strat =>
            log(s"$strat ...")(ds.toString)
            ds.write(s"INSERT OR IGNORE INTO p VALUES (NULL, ${strat.id}, 0, $run, $fold)")
            op(strat, ds, pool, run, fold)
            log(s"$strat ok.")(ds.toString)
          }

        }
      }
      end(ds)
      ds.close()
    }
    log("Datasets prontos.")()
  }
}
