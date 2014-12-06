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

import ml.Pattern
import util.Datasets
import weka.filters.Filter

import scala.util.Random

trait Exp extends AppWithUsage {
   val readOnly = false
   val ignoreNotDone: Boolean

   //  def strats(pool: Seq[Pattern], seed: Int): List[Strategy]

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter)

   def datasetFinished(ds: Ds)

   def isAlreadyDone(ds: Ds): Boolean

   /**
    * returns whether dataset was already done
    */
   override def run() = {
      super.run()
      memoryMonitor()
      val res = (if (parallelDatasets) datasets.toList.par else datasets.toList) map { dataset =>
         val ds = Ds(dataset, readOnly)
         ds.open()
         val res1 = if (isAlreadyDone(ds)) {
            println(s"$dataset already done!")
            ds.dataset -> true
         } else {
            if (!ignoreNotDone) {
               ds.log(s"Processing ${ds.n} instances ...")
               (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
                  val shuffled = new Random(run).shuffle(ds.patterns)
                  Datasets.kfoldCV(shuffled, k = folds, parallelFolds) { (tr, ts, fold, minSize) =>
                     ds.log(s"Pool $run.$fold (${tr.size} instances) ...")
                     val learnerSeed = run * 10000 + fold

                     //Ordena pool e testSet e cria filtros.
                     val pool = new Random(fold).shuffle(tr.sortBy(_.id))
                     val (fpool, binaf, zscof) = filterTr(tr, fold)

                     //ts
                     val testSet = new Random(fold).shuffle(ts.sortBy(_.id))
                     val ftestSet = filterTs(ts, fold, binaf, zscof)

                     //opera no ds // find (&& x.learner.id == strat.learner.id) desnecessario
                     op(ds, pool, testSet, fpool, ftestSet, learnerSeed, run, fold, binaf, zscof)
                  }
               }
               datasetFinished(ds)
            }
            ds.dataset -> false
         }
         ds.close()
         res1
      }
      end(res.toList.toMap)
      justQuit("Datasets prontos.\n" + args.toList)
   }

   def end(res: Map[String, Boolean])

   def filterTr(tr: Seq[Pattern], fold: Int) = {
      //bina
      val binaf = Datasets.binarizeFilter(tr)
      val binarizedTr = Datasets.applyFilter(binaf)(tr)

      //tr
      val zscof = Datasets.zscoreFilter(binarizedTr)
      val pool = {
         val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
         new Random(fold).shuffle(filteredTr.sortBy(_.id))
      }

      (pool, binaf, zscof)
   }

   def filterTs(ts: Seq[Pattern], fold: Int, binaf: Filter, zscof: Filter) = {
      //ts
      val binarizedTs = Datasets.applyFilter(binaf)(ts)
      val testSet = {
         val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
         new Random(fold).shuffle(filteredTs.sortBy(_.id))
      }
      testSet
   }
}
