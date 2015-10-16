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
import ml.classifiers.{NoLearner, MetaLearner}
import weka.filters.Filter

import scala.collection.mutable

object ameameta extends Exp with LearnerTrait with StratsTrait with RangeGenerator {
  val context = "ameametaApp"
  val arguments = superArguments :+ "leas(unused)" :+ "versao"
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
    val seqleas = learners(learnersStr) map (_.limpa)
    val fila = mutable.Set[String]()
    if (ds.nclasses > maxQueries(ds)) ds.error(s"ds.nclasses ${ds.nclasses} > ${maxQueries(ds)} maxtimesteps!")
    else if (ds.isAliveByOtherJob(run, fold)) {
      outroProcessoVaiTerminarEsteDataset = true
      ds.log(s"Outro job está all-izando este pool ($run.$fold). Skipping this pool...", 30)
    } else {
      ds.startbeat(run, fold)
      ds.log(s"Iniciando trabalho para pool $run.$fold ...", 30)
      val mapa = ((fpool ++ ftestSet) map (p => p.id -> p)) toMap
      val fmapa = ((fpool ++ ftestSet) map (p => p.id -> p)) toMap

      stratsPool("all", pool, pool).map { st =>
        //      (stratsPool("all", pool, pool) ++ stratsFpool(pool, fpool)).map { st =>
        val fakelearner = MetaLearner(pool, fpool, mapa, fmapa, learnerSeed, ds, st(NoLearner()), seqleas)("PCTr-a")
        val learner = MetaLearner(pool, fpool, mapa, fmapa, learnerSeed, ds, st(fakelearner), seqleas)("PCTr-a")
        learner -> st(learner)
      } foreach { case (metalea, strat) =>
        lazy val (tmin, _, tmax, _) = ranges(ds)
        for ((ti, tf) <- Seq((tmin, tmax), (tmin, 49), (50, tmax))) {
          poeNaFila(fila, ALCKappa(ds, strat, metalea, run, fold)(ti, tf).sqlToWrite(ds))
          poeNaFila(fila, ALCBalancedAcc(ds, strat, metalea, run, fold)(ti, tf).sqlToWrite(ds))
        }
        for (t <- tmin to tmax) {
          poeNaFila(fila, Kappa(ds, strat, metalea, run, fold)(t).sqlToWrite(ds))
          poeNaFila(fila, BalancedAcc(ds, strat, metalea, run, fold)(t).sqlToWrite(ds))
        }
      }
    }

    ds.log(fila.mkString("\n"), 10)
    if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
    fila.clear()
  }

  def datasetFinished(ds: Ds) = {
    if (acabou && !outroProcessoVaiTerminarEsteDataset) {
      ds.markAsFinishedRun("meta*lapack" + versao)
      ds.log("Dataset marcado como terminado !", 50)
    }
    outroProcessoVaiTerminarEsteDataset = false
    acabou = true
  }

  def isAlreadyDone(ds: Ds) = {
    val despreparado = if (!ds.isFinishedRun("meta-lapack" + versao) || !ds.isFinishedRun("meta+lapack" + versao)) {
      ds.log(s"acv ou acvf ainda não terminaram este dataset, skipping...", 30)
      true
    } else false
    despreparado || ds.isFinishedRun("meta*lapack" + versao)
  }

  def end(res: Map[String, Boolean]): Unit = {
  }
}
