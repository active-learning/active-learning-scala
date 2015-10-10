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

import clean.lib._
import ml.Pattern
import ml.classifiers.{BestClassifCVU2_10foldKappa, BestClassifCV50_10foldKappa, BestClassifCV100_10foldKappa, NoLearner}
import weka.filters.Filter

import scala.collection.mutable

object acv extends Exp with LearnerTrait with StratsTrait {
  val context = "acvApp"
  val arguments = superArguments
  val ignoreNotDone = false
  var outroProcessoVaiTerminarEsteDataset = false
  var acabou = true
  run()

  def poeNaFila(fila: mutable.Set[String], f: => String): Unit =
    try {
      fila += f
    } catch {
      case e: Throwable => acabou = false
    }

  def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
    val fila = mutable.Set[String]()
    if (ds.nclasses > maxQueries(ds)) ds.error(s"ds.nclasses ${ds.nclasses} > ${maxQueries(ds)} maxtimesteps!")
    else if (ds.isAliveByOtherJob(run, fold)) {
      outroProcessoVaiTerminarEsteDataset = true
      ds.log(s"Outro job está all-izando este pool ($run.$fold). Skipping all' for this pool...", 30)
    } else {
      ds.startbeat(run, fold)
      ds.log(s"Iniciando trabalho para pool $run.$fold ...", 30)
      //         val best = BestPassiveClassif(ds, learnerSeed, pool)

      learnersPool(pool, learnerSeed) foreach { learner =>
        stratsPool("all", pool, pool).map(_(learner)) foreach { strat =>
          ds.log(s"$learner $strat ...")

          val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
            ds.log(s" Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(strat, run, fold, null, null)
          } else ds.writeQueries(strat, run, fold, maxQueries(ds))
          val fqueries = ds.queries(strat, run, fold, binaf, zscof)

          //                              learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed) foreach { classif => //todos leas só pra sbmmulti e qbc
          Seq(learner) foreach { classif =>
            if (classif.querFiltro) {
              ds.log(s"fHits [$learner $strat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(fpool.size, ftestSet, strat, classif, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            } else {
              ds.log(s"Hits [$learner $strat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(pool.size, testSet, strat, classif, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            }
          }
        }

        if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
        fila.clear()
      }

      learnersFpool(learnerSeed) foreach { flearner =>
        stratsPool("all", fpool, pool).map(_(flearner)) foreach { strat =>
          ds.log(s"$flearner $strat ...")

          val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
            ds.log(s" Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(strat, run, fold, null, null)
          } else ds.writeQueries(strat, run, fold, maxQueries(ds))
          val fqueries = ds.queries(strat, run, fold, binaf, zscof)

          Seq(flearner) foreach { classif =>
            //               learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed) foreach { classif =>
            if (classif.querFiltro) {
              ds.log(s"fHits [$flearner $strat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(fpool.size, ftestSet, strat, classif, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            } else {
              ds.log(s"Hits [$flearner $strat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(pool.size, testSet, strat, classif, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            }
          }
        }

        if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
        fila.clear()
      }
    }
  }

  def datasetFinished(ds: Ds) = {
    if (acabou && !outroProcessoVaiTerminarEsteDataset) {
      ds.markAsFinishedRun("-lapack2")
      ds.log("Dataset marcado como terminado !", 50)
    }
    outroProcessoVaiTerminarEsteDataset = false
    acabou = true
  }

  def isAlreadyDone(ds: Ds) = ds.isFinishedRun("-lapack2")

  def end(res: Map[String, Boolean]): Unit = {
  }
}
