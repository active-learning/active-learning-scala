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
import ml.classifiers.{BestClassifCV100, NoLearner}
import weka.filters.Filter

object acv extends Exp with LearnerTrait with StratsTrait {
   val context = "acvApp"
   val arguments = superArguments
   val ignoreNotDone = false
   var outroProcessoVaiTerminarEsteDataset = false
   var acabou = true
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
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

               Seq(learner) foreach { classif =>
                  //               Seq(BestClassifCV(ds, run, fold, strat, queries, fqueries, learnerSeed, pool), best, learner) foreach { classif =>
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

               if (pool.size >= 200) {
                  val (qt100, fqt100) = (queries.take(100), fqueries.take(100))
                  val classif = BestClassifCV100(ds, run, fold, strat, qt100, fqt100, learnerSeed, pool)
                  val k = Kappa(ds, strat, classif, run, fold, true)(-1)
                  val b = BalancedAcc(ds, strat, classif, run, fold, true)(-1)
                  try {
                     if (!k.existia || !b.existia) {
                        val m = classif.build(if (classif.querFiltro) fqt100 else qt100)
                        val CM = m.confusion(if (classif.querFiltro) ftestSet else testSet)
                        k.write(ds, CM)
                        b.write(ds, CM)
                     }
                  } catch {
                     case e: NoPidForNonPassive => log(e.getMessage, 30)
                        acabou = false
                  }
               }
            }

            stratsFpool(pool, fpool).map(_(learner)) foreach { fstrat =>
               ds.log(s"$learner $fstrat ...")

               val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
                  ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
                  ds.queries(fstrat, run, fold, binaf, zscof)
               } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
               val queries = ds.queries(fstrat, run, fold, null, null)

               Seq(learner) foreach { classif =>
                  //               Seq(BestClassifCV(ds, run, fold, fstrat, queries, fqueries, learnerSeed, pool), best, learner) foreach { classif =>
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

               if (pool.size >= 200) {
                  val (qt100, fqt100) = (queries.take(100), fqueries.take(100))
                  val classif = BestClassifCV100(ds, run, fold, fstrat, qt100, fqt100, learnerSeed, pool)
                  val k = Kappa(ds, fstrat, classif, run, fold, true)(-1)
                  val b = BalancedAcc(ds, fstrat, classif, run, fold, true)(-1)
                  try {
                     if (!k.existia || !b.existia) {
                        val m = classif.build(if (classif.querFiltro) fqt100 else qt100)
                        val CM = m.confusion(if (classif.querFiltro) ftestSet else testSet)
                        k.write(ds, CM)
                        b.write(ds, CM)
                     }
                  } catch {
                     case e: NoPidForNonPassive => log(e.getMessage, 30)
                        acabou = false
                  }
               }
            }
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
                  //               Seq(BestClassifCV(ds, run, fold, strat, queries, fqueries, learnerSeed, pool), best, learner) foreach { classif =>
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

               if (pool.size >= 200) {
                  val (qt100, fqt100) = (queries.take(100), fqueries.take(100))
                  val classif = BestClassifCV100(ds, run, fold, strat, qt100, fqt100, learnerSeed, pool)
                  val k = Kappa(ds, strat, classif, run, fold, true)(-1)
                  val b = BalancedAcc(ds, strat, classif, run, fold, true)(-1)
                  try {
                     if (!k.existia || !b.existia) {
                        val m = classif.build(if (classif.querFiltro) fqt100 else qt100)
                        val CM = m.confusion(if (classif.querFiltro) ftestSet else testSet)
                        k.write(ds, CM)
                        b.write(ds, CM)
                     }
                  } catch {
                     case e: NoPidForNonPassive => log(e.getMessage, 30)
                        acabou = false
                  }
               }
            }

            stratsFpool(fpool, fpool).map(_(flearner)) foreach { fstrat =>
               ds.log(s"$flearner $fstrat ...")

               val fqueries = if (ds.areQueriesFinished(fpool.size, fstrat, run, fold, binaf, zscof, completeIt = true, maxQueries(ds))) {
                  ds.log(s"fQueries  done for ${fstrat.abr}/${fstrat.learner} at pool $run.$fold. Retrieving from disk.")
                  ds.queries(fstrat, run, fold, binaf, zscof)
               } else ds.writeQueries(fstrat, run, fold, maxQueries(ds))
               val queries = ds.queries(fstrat, run, fold, null, null)

               Seq(flearner) foreach { classif =>
                  //               Seq(BestClassifCV(ds, run, fold, fstrat, queries, fqueries, learnerSeed, pool), best, learner) foreach { classif =>
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

               if (pool.size >= 200) {
                  val (qt100, fqt100) = (queries.take(100), fqueries.take(100))
                  val classif = BestClassifCV100(ds, run, fold, fstrat, qt100, fqt100, learnerSeed, pool)
                  val k = Kappa(ds, fstrat, classif, run, fold, true)(-1)
                  val b = BalancedAcc(ds, fstrat, classif, run, fold, true)(-1)
                  try {
                     if (!k.existia || !b.existia) {
                        val m = classif.build(if (classif.querFiltro) fqt100 else qt100)
                        val CM = m.confusion(if (classif.querFiltro) ftestSet else testSet)
                        k.write(ds, CM)
                        b.write(ds, CM)
                     }
                  } catch {
                     case e: NoPidForNonPassive => log(e.getMessage, 30)
                        acabou = false
                  }
               }
            }
         }
      }
   }

   def datasetFinished(ds: Ds) = {
      if (acabou && !outroProcessoVaiTerminarEsteDataset) {
         ds.markAsFinishedRun("acv11" + (stratsFpool().map(_(NoLearner())) ++ stratsPool("all").map(_(NoLearner())) ++ allLearners()).map(x => x.limpa).mkString)
         ds.log("Dataset marcado como terminado !", 50)
      }
      outroProcessoVaiTerminarEsteDataset = false
      acabou = true
   }

   def isAlreadyDone(ds: Ds) = ds.isFinishedRun("acv11" + (stratsFpool().map(_(NoLearner())) ++ stratsPool("all").map(_(NoLearner())) ++ allLearners()).map(x => x.limpa).mkString)

   def end(res: Map[String, Boolean]): Unit = {
   }
}
