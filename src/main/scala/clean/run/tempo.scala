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
import util.Tempo

object tempo extends AppWithUsage with LearnerTrait with StratsTrait with MeasuresTrait {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "tempo"
   val qs = 1000
   run()

   override def run() = {
      super.run()
      datasets.toList.foreach { dataset =>
         val ds = Ds(dataset)
         ds.open()
         0 until runs foreach { r =>
            val pool = new scala.util.Random(r).shuffle(ds.patterns).take(ds.expectedPoolSizes(folds).min)
            learners(learnersStr).foreach { learner =>
               val sqls = allStrats(learner, pool) flatMap { strat =>
                  val t = Tempo.time {
                     strat.queries.take(qs)
                  } / qs
                  Seq(s"insert into p values (${strat.id}, ${learner.id}, $r, -1)",
                     s"insert into r select ${1000 + qs}, id, $t from p where s=${strat.id} and l=${learner.id} and r=$r and f=-1") //tempo id = 1000 + #queries
               }
               ds.batchWrite(sqls)
            }
         }
         ds.close()
      }
   }
}

//micro-mass-mixed-spectra,digits2-davi,micro-mass-pure-spectra,musk,multiple-features
