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

import al.strategies.{SVMmulti, Majoritary, Passive}
import clean._
import clean.meta.RangeGenerator
import clean.res._
import ml.Pattern
import ml.classifiers.{SVMLib, Maj}
import weka.filters.Filter

import scala.collection.mutable

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      val fila = mutable.Set[String]()
      //passiva
      for (learner <- learnersFilterFree(pool, rnd.nextInt(99999))) {
         val k = Kappa(ds, Passive(pool), learner, run, fold)(-1)
         val b = BalancedAcc(ds, Passive(pool), learner, run, fold)(-1)
         if (!k.existia || !b.existia) {
            val model = learner.build(pool)
            val CM = model.confusion(testSet)
            fila += k.sqlToWrite(ds, CM)
            fila += b.sqlToWrite(ds, CM)
         }
      }
      for (flearner <- learnersFilterDependent(rnd.nextInt(99999))) {
         val k = Kappa(ds, Passive(fpool), flearner, run, fold)(-1)
         val b = BalancedAcc(ds, Passive(fpool), flearner, run, fold)(-1)
         if (!k.existia || !b.existia) {
            val model = flearner.build(fpool)
            val CM = model.confusion(ftestSet)
            fila += k.sqlToWrite(ds, CM)
            fila += b.sqlToWrite(ds, CM)
         }
      }
      if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
      fila.clear()

      //majoritaria e svm
      for ((ti, tf) <- maxRange(ds, 2, 100) +: maxRange(ds, 2, 200) +: (ranges(ds, 2, 100) ++ ranges(ds, 2, 200))) {
         fila += ALCKappa(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).sqlToWrite(ds)
         fila += ALCBalancedAcc(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).sqlToWrite(ds)
         fila += ALCKappa(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds)
         fila += ALCBalancedAcc(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds)
      }
      val (i, f) = maxRange(ds, 2, 200)
      for (t <- i to f) {
         fila += Kappa(ds, Majoritary(Seq()), Maj(), run, fold)(t).sqlToWrite(ds)
         fila += BalancedAcc(ds, Majoritary(Seq()), Maj(), run, fold)(t).sqlToWrite(ds)
         fila += Kappa(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds)
         fila += BalancedAcc(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds)
      }

      //outras
      for (strat <- allStrats(); learner <- allLearners(); (ti, tf) <- maxRange(ds, 2, 100) +: maxRange(ds, 2, 200) +: (ranges(ds, 2, 100) ++ ranges(ds, 2, 200))) {
         strat match {
            case Majoritary(Seq(), false) | SVMmulti(Seq(), "KFFw", false) => //jah foi acima
            case s =>
               fila += ALCKappa(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds)
               fila += ALCBalancedAcc(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds)
         }
      }
      for (strat <- allStrats(); learner <- allLearners(); t <- i to f) {
         strat match {
            case Majoritary(Seq(), false) | SVMmulti(Seq(), "KFFw", false) => //jah foi acima
            case s =>
               fila += Kappa(ds, s, learner, run, fold)(i).sqlToWrite(ds)
               fila += BalancedAcc(ds, s, learner, run, fold)(f).sqlToWrite(ds)
         }
      }

      if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
      fila.clear()
   }

   def datasetFinished(ds: Ds) {
   }

   def isAlreadyDone(ds: Ds) = false //ds.isMeasureComplete(measure, s.id, learner.id)

   def end(res: Map[String, Boolean]): Unit = {
      println(s"prontos. \n${args.toList}")
   }
}
