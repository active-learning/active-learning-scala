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

import clean._
import ml.Pattern
import weka.filters.Filter

object all extends Exp with LearnerTrait with StratsTrait {
   val context = "allApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      if (ds.nclasses > maxQueries(ds)) ds.error(s"ds.nclasses ${ds.nclasses} > ${maxQueries(ds)} maxtimesteps!")
      else if (ds.isAliveByOtherJob()) ds.log("Outro job está all-izando este dataset. Skipping all' for this pool...", 30)
      else {
         ds.startbeat()

         //rnd clu svm maj / lff lfd
         stratsFilterFreeSemLearnerExterno(pool).zip(stratsFilterFreeSemLearnerExterno(fpool)) foreach { case (strat, fstrat) =>
            ds.log(s"$strat ...")
            //queries
            val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
               ds.log(s"agn, SVM and maj Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
               ds.queries(strat, run, fold, null, null)
            } else ds.writeQueries(strat, run, fold, maxQueries(ds))
            val fqueries = ds.queries(fstrat, run, fold, binaf, zscof)
            //hits
            if (strat.id >= 17 && strat.id <= 21) {
               val learner = strat.learner
               ds.log(s"SVM/Maj hits [$strat $learner] at pool $run.$fold.", 20)
               if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
               else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
            } else {
               learnersFilterFree(pool, learnerSeed) foreach { learner =>
                  ds.log(s"Agn hits [$strat $learner] at pool $run.$fold.", 20)
                  if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
                  else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
               }
               learnersFilterDependent(learnerSeed) foreach { flearner =>
                  ds.log(s"Agnf hits [$fstrat $flearner] at pool $run.$fold.", 20)
                  if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
                  else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
               }
            }
            ds.log(s"$strat ok.")
         }

         //restoSemF / lff
         learnersFilterFree(pool, learnerSeed) foreach { learner =>
            stratsFilterFreeComLearnerExterno(pool, learner) foreach { case strat =>
               ds.log(s"$strat ...")
               //queries
               val queries = if (ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = true, maxQueries(ds))) {
                  ds.log(s"nonf Queries  done for ${strat.abr}/${strat.learner} at pool $run.$fold. Retrieving from disk.")
                  ds.queries(strat, run, fold, null, null)
               } else ds.writeQueries(strat, run, fold, maxQueries(ds))
               //hits
               ds.log(s"nonFilter hits [$strat $learner] at pool $run.$fold.", 20)
               if (ds.areHitsFinished(pool.size, testSet, strat, learner, run, fold, null, null, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"Hits  done for ${strat.abr}/$learner at pool $run.$fold.")
               else ds.writeHits(pool.size, testSet, queries.toVector, strat, run, fold, maxQueries(ds) - ds.nclasses + 1)(learner)
               ds.log(s"$strat ok.")
            }
         }

         //restoSemF / lfd
         learnersFilterDependent(learnerSeed) foreach { flearner =>
            stratsFilterFreeComLearnerExterno(fpool, flearner) foreach { case fstrat =>
               ds.log(s"$fstrat ...")
               //queries
               val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
                  ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
                  ds.queries(fstrat, run, fold, binaf, zscof)
               } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))

               //hits
               ds.log(s"Filter hits [$fstrat $flearner] at pool $run.$fold.", 20)
               if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
               else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
               ds.log(s"$fstrat ok.")
            }
         }

         //restoComF / lff lfd
         (learnersFilterFree(fpool, learnerSeed) ++ learnersFilterDependent(learnerSeed)) foreach { flearner =>
            stratsFilterDependentComLearnerExterno(fpool, flearner) foreach { case fstrat =>
               ds.log(s"$fstrat ...")
               //queries
               val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
                  ds.log(s"fQueries maha  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
                  ds.queries(fstrat, run, fold, binaf, zscof)
               } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))

               //hits
               ds.log(s"Filter maha hits [$fstrat $flearner] at pool $run.$fold.", 20)
               if (ds.areHitsFinished(fpool.size, ftestSet, fstrat, flearner, run, fold, binaf, zscof, completeIt = true, maxQueries(ds) - ds.nclasses + 1)) ds.log(s"fHits  done for ${fstrat.abr}/$flearner at pool $run.$fold.")
               else ds.writeHits(fpool.size, ftestSet, fqueries.toVector, fstrat, run, fold, maxQueries(ds) - ds.nclasses + 1)(flearner)
               ds.log(s"$fstrat ok.")
            }
         }
      }
   }

   def datasetFinished(ds: Ds) {
   }

   def isAlreadyDone(ds: Ds) = false

   def end(res: Map[String, Boolean]): Unit = {
   }
}
