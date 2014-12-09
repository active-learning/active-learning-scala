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

import al.strategies.Passive
import clean._
import clean.meta.RangeGenerator
import clean.res._
import ml.Pattern
import weka.filters.Filter

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      //passiva
      for {
         learner <- learnersFilterFree(pool, rnd.nextInt(99999))
      } yield {
         val model = learner.build(pool)
         val CM = model.confusion(testSet)
         Kappa(ds, Passive(pool), learner, run, fold)(-1).write(ds, CM)
         BalancedAcc(ds, Passive(pool), learner, run, fold)(-1).write(ds, CM)
      }
      for {
         flearner <- learnersFilterDependent(rnd.nextInt(99999))
      } yield {
         val model = flearner.build(fpool)
         val CM = model.confusion(ftestSet)
         Kappa(ds, Passive(fpool), flearner, run, fold)(-1).write(ds, CM)
         BalancedAcc(ds, Passive(fpool), flearner, run, fold)(-1).write(ds, CM)
      }

      for {
         strat <- allStrats()
         learner <- allLearners()
         (budix, ti, tf) <- ranges(ds)
      } yield {
         val model = learner.build(pool)
         val CM = model.confusion(testSet)
         ALCKappa(ds, Passive(pool), learner, run, fold)(ti, tf).write(ds)
      }

      //      stratsemLearnerExterno() foreach { strat =>
      //         if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
      //            ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
      //         } else {
      //            //estratégias de learner único (Maj/ZeroR id = 21) (enquanto estiver suando kappa, nao precisa de maj)
      //            //               if (strat.id >= 17 && strat.id <= 21 || strat.id == 969) {
      //            //                  val m = measu(ds, strat, strat.learner, run, fold,)
      //            //                  storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)(strat.learner)
      //            //               }               else
      //            allLearners() foreach storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)
      //         }
      //      }
      //      allLearners() foreach { learner =>
      //         stratcomLearnerExterno(learner) foreach { strat =>
      //            if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
      //               ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
      //            } else storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)(learner)
      //         }
      //      }
   }

   def datasetFinished(ds: Ds) {
   }

   def isAlreadyDone(ds: Ds) = false //ds.isMeasureComplete(measure, s.id, learner.id)

   def end(res: Map[String, Boolean]): Unit = {
      println(s"prontos. \n${args.toList}")
   }
}
