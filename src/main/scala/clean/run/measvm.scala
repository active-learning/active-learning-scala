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

import al.strategies.{Majoritary, Passive, SVMmultiLinear}
import clean.lib._
import ml.Pattern
import ml.classifiers._
import weka.filters.Filter

import scala.collection.mutable

object measvm extends Exp with LearnerTrait with StratsTrait with Lock with CM with RangeGenerator {
   val context = "measvmApp"
   val arguments = superArguments ++ Seq("p:passivas")
   val ignoreNotDone = false
   var acabou = false
   val strats = stratsSemLearnerExterno_FilterFree(Seq()).dropRight(1) ++ stratsSemLearnerExterno_FilterDependent(Seq()).dropRight(1) ++ stratsComLearnerExterno_FilterFree(Seq(),NoLearner()) ++ stratsComLearnerExterno_FilterDependent(Seq(),NoLearner())
   run()

   def poeNaFila(fila: mutable.Set[String], f: => String): Unit =
      try {
         fila += f
      } catch {
         case e: Throwable =>
      }

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      val fila = mutable.Set[String]()
      //      //passiva
      if (passivas) {
         val fpoolt = fpool.take(3000)
         for (flearner <- Seq(SVMLibRBF(learnerSeed))) {
            val k = Kappa(ds, Passive(fpoolt), flearner, run, fold)(-1)
            val b = BalancedAcc(ds, Passive(fpoolt), flearner, run, fold)(-1)
            if (!k.existia || !b.existia) {
               val model = flearner.build(fpoolt)
               val CM = model.confusion(ftestSet)
               poeNaFila(fila, k.sqlToWrite(ds, CM))
               poeNaFila(fila, b.sqlToWrite(ds, CM))
               if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
               fila.clear()
            }
         }
         if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
         fila.clear()
      } else {
         lazy val (tmin, thalf, tmax, tpass) = ranges(ds)
         for (strat <- strats; learner <- Seq(SVMLibRBF(learnerSeed)); (ti, tf) <- Seq((tmin, thalf), (thalf, tmax), (tmin, tmax), (tmin, 49))) {
            strat match {
               case Majoritary(Seq(), false) => //| SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
               case s =>
                  if (!Global.gnosticasComLearnerInterno.contains(strat.id) || (strat.id == 1006600 && learner.id == 11) || (strat.id == 1292212 && learner.id == 773) || (Seq(966000, 967000, 968000, 969000).contains(strat.id) && Seq(165111, 556665).contains(learner.id)) || (Seq(966009, 967009, 968009, 969009).contains(strat.id) && learner.id == 2651110)) {
                     poeNaFila(fila, ALCKappa(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds))
                     poeNaFila(fila, ALCBalancedAcc(ds, s, learner, run, fold)(ti, tf).sqlToWrite(ds))
                  }
            }
         }
         for (strat <- strats; learner <- Seq(SVMLibRBF(learnerSeed)); t <- tmin to tmax) {
            strat match {
               case Majoritary(Seq(), false) => //| SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
               case s =>
                  if (!Global.gnosticasComLearnerInterno.contains(strat.id) || (strat.id == 1006600 && learner.id == 11) || (strat.id == 1292212 && learner.id == 773) || (Seq(966000, 967000, 968000, 969000).contains(strat.id) && Seq(165111, 556665).contains(learner.id)) || (Seq(966009, 967009, 968009, 969009).contains(strat.id) && learner.id == 2651110)) poeNaFila(fila, Kappa(ds, s, learner, run, fold)(t).sqlToWrite(ds))
            }
         }
         for (strat <- strats; learner <- Seq(SVMLibRBF(learnerSeed))) {
            val t = tpass
            strat match {
               case Majoritary(Seq(), false) => // | SVMmulti(Seq(), "KFFw", false) | SVMmulti(Seq(), "BALANCED_EEw", false) => //jah foi acima
               case s =>
                  if (!Global.gnosticasComLearnerInterno.contains(strat.id) || (strat.id == 1006600 && learner.id == 11) || (strat.id == 1292212 && learner.id == 773) || (Seq(966000, 967000, 968000, 969000).contains(strat.id) && Seq(165111, 556665).contains(learner.id)) || (Seq(966009, 967009, 968009, 969009).contains(strat.id) && learner.id == 2651110)) poeNaFila(fila, BalancedAcc(ds, s, learner, run, fold)(t).sqlToWrite(ds))
            }
         }
         println(s"")
         println(s"---------------------")
         fila foreach println
         println(s"^^^^^^^^^^^^^^^^^^")
         println(s"")
         if (fila.exists(_.startsWith("insert"))) ds.batchWrite(fila.toList)
         else if (fila.count(_ != "select 7") == 0) acabou = true
         fila.clear()
      }
   }

   def datasetFinished(ds: Ds) {
      if (acabou) {
         ds.markAsFinishedMea(passivas + strats.map(_.limpa).mkString + Seq(SVMLibRBF()).map(_.limpa).mkString)
         ds.log("Dataset marcado como terminado !", 50)
      }
   }

   def isAlreadyDone(ds: Ds) = {
      acabou = false
      ds.isFinishedMea(passivas + strats.map(_.limpa).mkString + Seq(SVMLibRBF()).map(_.limpa).mkString)
   }

   def end(res: Map[String, Boolean]): Unit = {
      println(s"prontos. \n${args.toList}")
   }
}
