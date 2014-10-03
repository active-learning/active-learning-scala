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

import al.strategies.{DensityWeightedTrainingUtility, MahalaWeightedTrainingUtility, Strategy}
import ml.Pattern
import ml.neural.elm.ELM
import util.Datasets
import weka.filters.Filter

import scala.util.Random

trait Exp extends AppWithUsage {
  val runs = Global.runs
  val folds = Global.folds

  def strats(pool: Seq[Pattern], seed: Int): List[Strategy]

  def op(strat: Strategy, ds: Ds, pool: Seq[Pattern], learnerSeed: Int, testSet: Seq[Pattern], run: Int, fold: Int, binaf: Filter, zscof: Filter)

  def datasetClosing(ds: Ds)

  def isAlreadyDone(ds: Ds): Boolean

  override def init() {
    super.init()
    memoryMonitor()
    datasets foreach { dataset =>
      val ds = Ds(path, dataset)
      ds.open()
      if (isAlreadyDone(ds)) println(s"$dataset already done!")
      else {
        ds.log(s"Processing ${ds.n} instances ...")
        (if (parallelRuns) (0 until runs).par else 0 until Global.runs) foreach { run =>
          val shuffled = new Random(run).shuffle(ds.patterns)
          Datasets.kfoldCV(shuffled, k = folds, parallelFolds) { (tr, ts, fold, minSize) =>
            ds.log(s"Pool $run.$fold (${tr.size} instances) ...")
            val learnerSeed = run * 10000 + fold
            strats(Seq(), learnerSeed) foreach { strat =>
              ds.log(s"$strat ...")

              //Ordena pool,testSet e aplica filtro se preciso.
              val needsFilter = (strat, strat.learner) match {
                case (_, _: ELM) => true
                case (DensityWeightedTrainingUtility(_, _, "maha", _, _, _), _) => true
                case (MahalaWeightedTrainingUtility(_, _, _, _, _), _) => true
                case _ => false
              }

              //bina
              val binaf = if (needsFilter) Datasets.binarizeFilter(tr) else null
              lazy val binarizedTr = Datasets.applyFilter(binaf)(tr)
              lazy val binarizedTs = Datasets.applyFilter(binaf)(ts)

              //tr
              val zscof = if (needsFilter) Datasets.zscoreFilter(binarizedTr) else null
              val pool = if (!needsFilter) new Random(fold).shuffle(tr.sortBy(_.id))
              else {
                val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
                new Random(fold).shuffle(filteredTr.sortBy(_.id))
              }

              //ts
              val testSet = if (!needsFilter) new Random(fold).shuffle(ts.sortBy(_.id))
              else {
                val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
                new Random(fold).shuffle(filteredTs.sortBy(_.id))
              }

              //opera no ds // find (&& x.learner.id == strat.learner.id) desnecessario
              op(strats(pool, learnerSeed).find(_.id == strat.id).get, ds, pool, learnerSeed, testSet, run, fold, binaf, zscof)

              ds.log(s"$strat ok.")
            }

          }
        }
        datasetClosing(ds)
      }
      ds.close()
    }
    end()
    log("Datasets prontos.", 20)
  }

  def end()
}
