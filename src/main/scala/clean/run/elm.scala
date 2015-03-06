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
import ml.classifiers.NinteraELM
import weka.filters.Filter

object elm extends Exp with LearnerTrait with StratsTrait {
   val context = "elmApp"
   val arguments = superArguments ++ Seq("p:pesadas")
   val ignoreNotDone = false
   var outroProcessoVaiTerminarEsteDataset = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      if (ds.nclasses > maxQueries(ds)) ds.error(s"ds.nclasses ${ds.nclasses} > ${maxQueries(ds)} maxtimesteps!")
      else if (ds.isAliveByOtherJob(run, fold)) {
         outroProcessoVaiTerminarEsteDataset = true
         ds.log(s"Outro job está all-izando este pool ($run.$fold). Skipping all' for this pool...", 30)
      } else {
         ds.startbeat(run, fold)
         ds.log(s"Iniciando trabalho para pool $run.$fold ...", 30)

         val flearner = NinteraELM(learnerSeed)
         stratsSemLearnerExterno_FilterDependent(fpool) foreach { fstrat =>
            ds.log(s"$fstrat ...")
            //queries
            val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
               ds.log(s"agDW* fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
               ds.queries(fstrat, run, fold, binaf, zscof)
            } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
            //hits
            if (Global.gnosticasComLearnerInterno.contains(fstrat.id)) {
               if (flearner.id == fstrat.learner.id) {
                  ds.log(s" hits [$fstrat $flearner] at pool $run.$fold.")
                  if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
                  else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
               }
            } else {
               ds.log(s"outros hits [$fstrat $flearner] at pool $run.$fold.")
               if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"agDW*  Hits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
               else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
            }
            ds.log(s"$fstrat ok.")
         }

         stratsComLearnerExterno_FilterFree(fpool, flearner) foreach { case fstrat =>
            ds.log(s"$fstrat ...")
            //queries
            val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
               ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
               ds.queries(fstrat, run, fold, binaf, zscof)
            } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))

            //hits
            ds.log(s"Filter hits [$fstrat $flearner] at pool $run.$fold.")
            if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
            else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
            ds.log(s"$fstrat ok.")
         }

         stratsComLearnerExterno_FilterDependent(fpool, flearner) foreach { case fstrat =>
            ds.log(s"$fstrat ...")
            //queries
            val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
               ds.log(s"fQueries maha  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
               ds.queries(fstrat, run, fold, binaf, zscof)
            } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))

            //hits
            ds.log(s"Filter maha hits [$fstrat $flearner] at pool $run.$fold.")
            if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
            else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
            ds.log(s"$fstrat ok.")
         }
      }
   }

   def datasetFinished(ds: Ds) = {
      if (!outroProcessoVaiTerminarEsteDataset) {
         if (todas) {
            ds.markAsFinished(maxQueries(ds))
            ds.log("Dataset marcado como terminado !", 50)
         }
      }
      outroProcessoVaiTerminarEsteDataset = false
   }

   def isAlreadyDone(ds: Ds) = ds.isFinished(maxQueries(ds))

   def end(res: Map[String, Boolean]): Unit = {
   }
}
