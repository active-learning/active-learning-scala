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
import ml.classifiers.NoLearner
import weka.filters.Filter

import scala.collection.mutable

object acvf extends Exp with LearnerTrait with StratsTrait {
  val context = "acvfApp"
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
        stratsFpool(pool, fpool).map(_(learner)) foreach { fstrat =>
          ds.log(s"$learner $fstrat ...")

          val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
            ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(fstrat, run, fold, binaf, zscof)
          } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
          val queries = ds.queries(fstrat, run, fold, null, null)

          Seq(learner) foreach { classif =>
            //               learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed) foreach { classif =>
            if (classif.querFiltro) {
              ds.log(s"fHits [$learner $fstrat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, classif, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            } else {
              ds.log(s"Hits [$learner $fstrat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(pool.size, testSet, fstrat, classif, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(pool.size, testSet, queries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            }
          }
        }
        if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
        fila.clear()
      }

      learnersFpool(learnerSeed) foreach { flearner =>
        stratsFpool(fpool, fpool).map(_(flearner)) foreach { fstrat =>
          ds.log(s"$flearner $fstrat ...")

          val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
            ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
            ds.queries(fstrat, run, fold, binaf, zscof)
          } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
          val queries = ds.queries(fstrat, run, fold, null, null)

          //                              learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed) foreach { classif =>
          Seq(flearner) foreach { classif =>
            if (classif.querFiltro) {
              ds.log(s"fHits [$flearner $fstrat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, classif, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
            } else {
              ds.log(s"Hits [$flearner $fstrat $classif] at pool $run.$fold.")
              if (ds.areHitsFinished(pool.size, testSet, fstrat, classif, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$classif at pool $run.$fold.")
              else ds.writeHits(pool.size, testSet, queries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(classif)
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
      ds.markAsFinishedRun("+lapack2")
      ds.log("Dataset marcado como terminado !", 50)
    }
    outroProcessoVaiTerminarEsteDataset = false
    acabou = true
  }

  def isAlreadyDone(ds: Ds) = ds.isFinishedRun("+lapack2")

  def end(res: Map[String, Boolean]): Unit = {
  }
}
