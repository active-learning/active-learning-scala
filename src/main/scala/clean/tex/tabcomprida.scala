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

package clean.tex

import clean._
import clean.res._
import util.{Stat, StatTests}

object tabcomprida extends AppWithUsage with LearnerTrait with StratsTrait with MeasuresTrait {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "tabcompridatex"
   run()

   override def run() = {
      super.run()
      Seq(timeToStart(maxQueries0), timeToQuery(maxQueries0), timeToLearn(maxQueries0), null, null).dropRight(2) foreach { measure =>
         //     Seq(ALCaccBal(maxQueries0), null, null).dropRight(2) foreach { measure =>
         //    allMeasures.dropRight(2) foreach { measure =>
         //      val strats = (measure.id match {
         //        case 11 => Seq(PassiveAcc(NoLearner(), Seq()))
         //        case 12 => Seq(PassiveGme(NoLearner(), Seq()))
         //        case _ => Seq()
         //      }) ++ allStrats()
         val strats = allStrats()
         val sl = strats.map(_.abr)

         val res0 = for {
            dataset <- datasets.toList.par
            l <- learners(learnersStr)
         } yield {
            val ds = Ds(dataset)
            ds.open()
            val sres = for {
               s <- strats
            } yield {
               val le = if (s.id >= 17 && s.id <= 21) s.learner else l
               val vv = if (ds.isMeasureComplete(measure, s.id, le.id)) {
                  val vs = for {
                     r <- 0 until runs
                     f <- 0 until folds
                  } yield {
                     if (measure.id(ds) == 0) ds.quit("nao sei")
                     else ds.getMeasure(measure, s, le, r, f) match {
                        case Some(v) => v
                        case None => ds.quit(s"No measure for ${(measure, s, le, r, f)}!")
                     }
                  }
                  Stat.media_desvioPadrao(vs.toVector)
               } else (Double.MinValue, Double.MinValue)
               vv
            }
            ds.close()
            //         "asd" -> sres
            (ds.dataset + l.toString.take(3)) -> sres
         }
         val res = res0.toList.sortBy(x => x._2.head)
         println(s"")
         println(s"")
         println(s"")

         val tbs = res.toList.sortBy(x => x._2.max) grouped 100
         //         val tbs = res.filter(!_._2.contains(-1d, -1d)).toList.sortBy(x => x._2.head) grouped 100
         tbs foreach { case res1 =>
            StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), sl.toVector.map(_.toString), "nomeTab", measure.toString)
         }
      }
   }
}