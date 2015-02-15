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

import al.strategies.{Majoritary, Passive, SVMmulti}
import clean.lib._
import ml.Pattern
import ml.classifiers.{Maj, SVMLib}
import weka.filters.Filter

import scala.collection.mutable

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def poeNaFila(fila: mutable.Set[String], f: => String): Unit = try {
      fila += f
   } catch {
      case e: Throwable =>
   }

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      val fila = mutable.Set[String]()
      //passiva
      //      for (learner <- learnersFilterFree(pool, rnd.nextInt(99999))) {
      //         val k = Kappa(ds, Passive(pool), learner, run, fold)(-1)
      //         val b = BalancedAcc(ds, Passive(pool), learner, run, fold)(-1)
      //         if (!k.existia || !b.existia) {
      //            val model = learner.build(pool)
      //            val CM = model.confusion(testSet)
      //            poeNaFila(fila, k.sqlToWrite(ds, CM))
      //            poeNaFila(fila, b.sqlToWrite(ds, CM))
      //         }
      //      }
      //      for (flearner <- learnersFilterDependent(rnd.nextInt(99999))) {
      //         val k = Kappa(ds, Passive(fpool), flearner, run, fold)(-1)
      //         val b = BalancedAcc(ds, Passive(fpool), flearner, run, fold)(-1)
      //         if (!k.existia || !b.existia) {
      //            val model = flearner.build(fpool)
      //            val CM = model.confusion(ftestSet)
      //            poeNaFila(fila, k.sqlToWrite(ds, CM))
      //            poeNaFila(fila, b.sqlToWrite(ds, CM))
      //         }
      //      }
      //      if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
      //      fila.clear()

      lazy val (tmin, thalf, tmax, tpass) = ranges(ds)

      //      //majoritaria e svm
      //      for ((ti, tf) <- Seq((tmin, thalf), (thalf, tmax), (tmin, tmax), (tmin, 50))) {
      //         poeNaFila(fila, ALCKappa(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).sqlToWrite(ds))
      //         poeNaFila(fila, ALCBalancedAcc(ds, Majoritary(Seq()), Maj(), run, fold)(ti, tf).sqlToWrite(ds))
      //         poeNaFila(fila, ALCKappa(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds))
      //         poeNaFila(fila, ALCBalancedAcc(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds))
      //         poeNaFila(fila, ALCKappa(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds))
      //         poeNaFila(fila, ALCBalancedAcc(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(ti, tf).sqlToWrite(ds))
      //      }
      //      for (t <- tmin to tmax) {
      //         poeNaFila(fila, Kappa(ds, Majoritary(Seq()), Maj(), run, fold)(t).sqlToWrite(ds))
      //         poeNaFila(fila, BalancedAcc(ds, Majoritary(Seq()), Maj(), run, fold)(t).sqlToWrite(ds))
      //         poeNaFila(fila, Kappa(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      //         poeNaFila(fila, BalancedAcc(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      //         poeNaFila(fila, Kappa(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      //         poeNaFila(fila, BalancedAcc(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      //      }

      //outras
      for (strat <- allStrats(); learner <- allLearners(); (ti, tf) <- Seq((tmin, thalf), (thalf, tmax), (tmin, tmax), (tmin, 50))) {
         strat match {
            case Majoritary(Seq(), false) | SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
            case s =>
               poeNaFila(fila, ALCKappa(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds))
            //               poeNaFila(fila, ALCBalancedAcc(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds))
         }
      }
      //      for (strat <- allStrats(); learner <- allLearners(); t <- tmin to tmax) {
      for (strat <- allStrats(); learner <- allLearners()) {
         val t = tpass
         strat match {
            case Majoritary(Seq(), false) | SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
            case s =>
               //               poeNaFila(fila, Kappa(ds, s, learner, run, fold)(t).sqlToWrite(ds))
               poeNaFila(fila, BalancedAcc(ds, s, learner, run, fold)(t).sqlToWrite(ds))
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
