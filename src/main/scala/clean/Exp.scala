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

import al.strategies.{MahalaWeightedTrainingUtility, DensityWeightedTrainingUtility, Strategy}
import ml.Pattern
import ml.neural.elm.ELM
import util.Datasets

import scala.io.Source
import scala.util.Random

trait Exp extends AppWithUsage {
  lazy val path = args(0)
  lazy val datasets = Source.fromFile(args(1)).getLines().filter(_.length > 2).filter(!_.startsWith("#"))
  val parallelRuns: Boolean
  val parallelFolds: Boolean

  def strats(pool: Seq[Pattern], seed: Int): List[Strategy]

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int)

  def end(ds: Ds)

  override def init() {
    super.init()
    datasets foreach { dataset =>
      val ds = Ds(path, Global.debug)(dataset)
      ds.open()

      //ajeita tabela h
      if (ds.read("SELECT count(1) from h").head.head == 0) {
        ds.write("DROP TABLE h")
        ds.write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) )")
      }

      log(s"Processing ${ds.n} instances ...")(ds.toString)
      (if (parallelRuns) (0 until Global.runs).par else 0 until Global.runs) foreach { run =>
        val shuffled = new Random(run).shuffle(ds.patterns)
        Datasets.kfoldCV(shuffled, k = Global.folds, parallelFolds) { (tr, ts, fold, minSize) =>
          log(s"Pool $run.$fold (${tr.size} instances) ...")(ds.toString)
          val learnerSeed = run * 10000 + fold
          strats(Seq(), learnerSeed) foreach { strat =>
            log(s"$strat ...")(ds.toString)

            //Ordena pool,testSet e aplica filtro se preciso.
            val needsFilter = (strat, strat.learner) match {
              case (_, _: ELM) => true
              case (DensityWeightedTrainingUtility(_, _, "maha", _, _, _), _) => true
              case (_: MahalaWeightedTrainingUtility, _) => true
              case _ => false
            }
            val (pool, testSet) = if (needsFilter) (new Random(fold).shuffle(tr.sortBy(_.id)), new Random(fold).shuffle(ts.sortBy(_.id)))
            else {
              val binaf = Datasets.binarizeFilter(tr)
              val binarizedTr = Datasets.applyFilter(binaf)(tr)
              val binarizedTs = Datasets.applyFilter(binaf)(ts)

              val zscof = Datasets.zscoreFilter(binarizedTr)
              val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
              val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)

              (new Random(fold).shuffle(filteredTr.sortBy(_.id)), new Random(fold).shuffle(filteredTs.sortBy(_.id)))
            }

            //opera no ds
            op(strats(pool, learnerSeed).find(_.id == strat.id).get, ds, pool, learnerSeed, testSet, run, fold)

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
