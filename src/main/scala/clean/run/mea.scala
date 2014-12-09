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
import clean.res._
import ml.Pattern
import weka.filters.Filter

import scala.collection.mutable

object mea extends Exp with LearnerTrait with StratsTrait with Lock with CM with MeasuresTrait {
   val context = "meaApp"
   val arguments = superArguments
   val ignoreNotDone = false
   run()

   def op(ds: Ds, pool: Seq[Pattern], testSet: Seq[Pattern], fpool: Seq[Pattern], ftestSet: Seq[Pattern], learnerSeed: Int, run: Int, fold: Int, binaf: Filter, zscof: Filter) {
      //      allMeasures foreach { measu =>
      //         //se for passiva, só poderia medir em special learners; mas posso rodar o classificador aqui com o pool inteiro e descobrir o valor
      //         //               specialLearners(Seq()) foreach storeSQL(pool.size, ds, RandomSampling(Seq()), run, fold, testSet.size, m)
      //         stratsemLearnerExterno() foreach { strat =>
      //            if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
      //               ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
      //            } else {
      //               //estratégias de learner único (Maj/ZeroR id = 21) (enquanto estiver suando kappa, nao precisa de maj)
      //               //               if (strat.id >= 17 && strat.id <= 21 || strat.id == 969) {
      //               //                  val m = measu(ds, strat, strat.learner, run, fold,)
      //               //                  storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)(strat.learner)
      //               //               }               else
      //               allLearners() foreach storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)
      //            }
      //         }
      //         allLearners() foreach { learner =>
      //            stratcomLearnerExterno(learner) foreach { strat =>
      //               if (!ds.areQueriesFinished(pool.size, strat, run, fold, null, null, completeIt = false, maxQueries(ds))) {
      //                  ds.log(s"Queries were not finished for ${strat.abr}/${strat.learner} at pool $run.$fold!")
      //               } else storeSQL(pool.size, ds, strat, run, fold, testSet.size, m)(learner)
      //            }
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
