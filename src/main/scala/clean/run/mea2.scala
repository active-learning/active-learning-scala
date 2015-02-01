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

object mea2 extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
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

      //svm
      val t = ranges(ds, 4, 400).apply(1)._2 //metade de U, mas limitado por 200
      ds.log(s"$t", 40)
      poeNaFila(fila, Kappa(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      poeNaFila(fila, BalancedAcc(ds, SVMmulti(Seq(), "KFFw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      poeNaFila(fila, Kappa(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(t).sqlToWrite(ds))
      poeNaFila(fila, BalancedAcc(ds, SVMmulti(Seq(), "BALANCED_EEw"), SVMLib(), run, fold)(t).sqlToWrite(ds))

      //outras
      for (strat <- allStrats(); learner <- allLearners()) {
         strat match {
            case Majoritary(Seq(), false) | SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
            case s =>
               poeNaFila(fila, Kappa(ds, s, learner, run, fold)(t).sqlToWrite(ds))
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
