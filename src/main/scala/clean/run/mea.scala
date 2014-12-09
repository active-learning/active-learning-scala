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

import al.strategies.{Majoritary, Passive}
import clean._
import clean.meta.RangeGenerator
import clean.res._
import ml.Pattern
import ml.classifiers.Maj
import weka.filters.Filter

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      //passiva
      for (learner <- learnersFilterFree(pool, rnd.nextInt(99999))) {
         val model = learner.build(pool)
         val CM = model.confusion(testSet)
         Kappa(ds, Passive(pool), learner, run, fold)(-1).write(ds, CM)
         BalancedAcc(ds, Passive(pool), learner, run, fold)(-1).write(ds, CM)
      }
      for (flearner <- learnersFilterDependent(rnd.nextInt(99999))) {
         val model = flearner.build(fpool)
         val CM = model.confusion(ftestSet)
         Kappa(ds, Passive(fpool), flearner, run, fold)(-1).write(ds, CM)
         BalancedAcc(ds, Passive(fpool), flearner, run, fold)(-1).write(ds, CM)
      }

      //majoritaria
      for ((budix, ti, tf) <- ranges(ds)) {
         ALCKappa(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).write(ds)
         ALCBalancedAcc(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).write(ds)
      }

      //outras
      for (strat <- allStrats(); learner <- allLearners(); (budix, ti, tf) <- ranges(ds)) {
         strat match {
            case Majoritary(Seq(), false) =>
            case _ =>
               ALCKappa(ds, Passive(pool), learner, run, fold)(ti, tf).write(ds)
               ALCBalancedAcc(ds, Passive(pool), learner, run, fold)(ti, tf).write(ds)
         }
      }
   }

   def datasetFinished(ds: Ds) {
   }

   def isAlreadyDone(ds: Ds) = false //ds.isMeasureComplete(measure, s.id, learner.id)

   def end(res: Map[String, Boolean]): Unit = {
      println(s"prontos. \n${args.toList}")
   }
}
