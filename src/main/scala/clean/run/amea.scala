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
import ml.classifiers._
import weka.filters.Filter

import scala.collection.mutable

object amea extends Exp with LearnerTrait with StratsTrait with RangeGenerator {
   val context = "amea4App"
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
         //         val best = BestPassiveClassif(ds, 42, Seq())
         for {
            learner <- learnersPool(pool, learnerSeed) ++ learnersFpool(learnerSeed)
            s <- stratsPool(pool, pool).map(_(learner)) ++ stratsFpool(pool, fpool).map(_(learner))
            classif <- Seq(learner)
         //            classif <- Seq(BestClassifCVReadOnly(ds, run, fold, s), best, learner)
         } yield {
            lazy val (tmin, thalf, tmax, tpass) = ranges(ds)
            for ((ti, tf) <- Seq((tmin, thalf), (thalf, tmax), (tmin, tmax), (tmin, 49))) {
               poeNaFila(fila, ALCKappa(ds, s, classif, run, fold)(ti, tf).sqlToWrite(ds))
               poeNaFila(fila, ALCBalancedAcc(ds, s, classif, run, fold)(ti, tf).sqlToWrite(ds))
            }
            for (t <- tmin to tmax) {
               poeNaFila(fila, Kappa(ds, s, classif, run, fold)(t).sqlToWrite(ds))
               poeNaFila(fila, BalancedAcc(ds, s, classif, run, fold)(t).sqlToWrite(ds))
            }
            val t = tpass
            poeNaFila(fila, Kappa(ds, s, classif, run, fold)(t).sqlToWrite(ds))
            poeNaFila(fila, BalancedAcc(ds, s, classif, run, fold)(t).sqlToWrite(ds))
         }
      }

      ds.log(fila.mkString("\n"), 10)
      if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
      fila.clear()
   }

   def datasetFinished(ds: Ds) = {
      if (acabou && !outroProcessoVaiTerminarEsteDataset) {
         ds.markAsFinishedRun("amea11" + (stratsFpool().map(_(NoLearner())) ++ stratsPool().map(_(NoLearner())) ++ allLearners()).map(x => x.limpa).mkString)
         ds.log("Dataset marcado como terminado !", 50)
      }
      outroProcessoVaiTerminarEsteDataset = false
      acabou = true
   }

   def isAlreadyDone(ds: Ds) = ds.isFinishedRun("amea11" + (stratsFpool().map(_(NoLearner())) ++ stratsPool().map(_(NoLearner())) ++ allLearners()).map(x => x.limpa).mkString)

   def end(res: Map[String, Boolean]): Unit = {
   }
}
