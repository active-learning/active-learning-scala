///*
//
//active-learning-scala: Active Learning library for Scala
//Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
//*/
//
//package clean.run
//
//import clean.lib._
//import ml.Pattern
//import ml.classifiers.RF
//import weka.filters.Filter
//
//object rf extends Exp with LearnerTrait with StratsTrait {
//   val context = "rfApp"
//   val arguments = superArguments
//   //++ Seq("p:pesadas")
//   val ignoreNotDone = false
//   var outroProcessoVaiTerminarEsteDataset = false
//   run()
//
//   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
//      if (ds.nclasses > maxQueries(ds)) ds.error(s"ds.nclasses ${ds.nclasses} > ${maxQueries(ds)} maxtimesteps!")
//      //      else if (ds.isAliveByOtherJob()) ds.log("Outro job está all-izando este dataset. Skipping all' for this pool...", 30)
//      else if (ds.isAliveByOtherJob(run, fold)) {
//         outroProcessoVaiTerminarEsteDataset = true
//         ds.log(s"Outro job está all-izando este pool ($run.$fold). Skipping all' for this pool...", 30)
//      } else {
//         ds.startbeat(run, fold)
//         ds.log(s"Iniciando trabalho para pool $run.$fold ...", 30)
//
//         val learner = RF(learnerSeed)
//         stratsSemLearnerExterno_FilterFree(pool) foreach { strat =>
//            ds.log(s"$strat ...")
//            //queries
//            val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
//               ds.log(s"agn, SVM and maj Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
//               ds.queries(strat, run, fold, null, null)
//            } else ds.writeQueries(strat, run, fold, maxQueries(ds))
//            //hits
//            ds.log(s"Agn hits [$strat $learner] at pool $run.$fold.")
//            if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
//            else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
//         }
//
//         stratsSemLearnerExterno_FilterDependent(fpool).dropRight(3) foreach { fstrat =>
//            ds.log(s"$fstrat ...")
//            //queries
//            val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
//               ds.log(s"agDW* fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
//               ds.queries(fstrat, run, fold, binaf, zscof)
//            } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
//            //hits
//            if (Global.gnosticasComLearnerInterno.contains(fstrat.id)) {
//               if (learner.id == fstrat.learner.id) {
//                  ds.log(s" hits [$fstrat $learner] at pool $run.$fold.")
//                  if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, learner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$learner at pool $run.$fold.")
//                  else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
//               }
//            } else {
//               ds.log(s"outros hits [$fstrat $learner] at pool $run.$fold.")
//               if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, learner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"agDW*  Hits  done for ${fstrat.abr}/$learner at pool $run.$fold.")
//               else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
//            }
//            ds.log(s"$fstrat ok.")
//         }
//
//         stratsComLearnerExterno_FilterFree(pool, learner) foreach { case strat =>
//            ds.log(s"$strat ...")
//            //queries
//            val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
//               ds.log(s"nonf Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
//               ds.queries(strat, run, fold, null, null)
//            } else ds.writeQueries(strat, run, fold, maxQueries(ds))
//            //hits
//            ds.log(s"nonFilter hits [$strat $learner] at pool $run.$fold.")
//            if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
//            else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
//            ds.log(s"$strat ok.")
//         }
//
//         stratsComLearnerExterno_FilterDependent(fpool, learner) foreach { case fstrat =>
//            ds.log(s"$fstrat ...")
//            //queries
//            val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
//               ds.log(s"fQueries maha  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
//               ds.queries(fstrat, run, fold, binaf, zscof)
//            } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
//
//            //hits
//            ds.log(s"Filter maha hits [$fstrat $learner] at pool $run.$fold.")
//            if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, learner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"fHits  done for ${fstrat.abr}/$learner at pool $run.$fold.")
//            else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
//            ds.log(s"$fstrat ok.")
//         }
//      }
//   }
//
//   def datasetFinished(ds: Ds) = {
//      if (!outroProcessoVaiTerminarEsteDataset) {
//         ds.markAsFinishedRun("rf" + allStrats().map(_.limpa).mkString + allLearners().map(_.limpa).mkString)
//         ds.log("Dataset marcado como terminado !", 50)
//      }
//      outroProcessoVaiTerminarEsteDataset = false
//   }
//
//   def isAlreadyDone(ds: Ds) = ds.isFinishedRun("rf" + allStrats().map(_.limpa).mkString + allLearners().map(_.limpa).mkString)
//
//   def end(res: Map[String, Boolean]): Unit = {
//   }
//}
