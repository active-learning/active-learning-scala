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

import clean.lib._
import util.StatTests

object tabwinners extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "tabwinnerstex"
   run()

   override def run() = {
      super.run()
      val measure = ALCBalancedAcc
      //      val measure = ALCKappa
      val strats = allStrats()
      val ss = strats.map(_.abr).toVector

      val datasetLearnerAndBoth = for {
         dataset <- datasets.toList
         l <- learners(learnersStr)
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         try {
            val vs = for {
               t <- Seq(0, 1)
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               val poolStr = (100 * r + f).toString
               val sres = for {
                  s <- strats
               } yield {
                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968 || s.id == 969) s.learner else l
                  val (ti, tf) = maxRange(ds, 2, 100) //<- verificar se põe 200
                  measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(-2d)
               }
               poolStr -> sres
            }
            Some((ds.dataset + l.toString.take(3)) -> StatTests.winners(vs, ss), (ds.dataset + l.toString.take(3)) -> StatTests.losers(vs, ss))
         } catch {
            case _: Throwable => None
         } finally {
            ds.close()
         }
      }

      val (datasetLearnerAndWinners, datasetLearnerAndLosers) = datasetLearnerAndBoth.flatten.unzip
      println(s"${datasetLearnerAndBoth.size} tests.")
      println(s"--------$measure---------------")
      //      datasetLearnerAndWinners foreach println
      val flat = datasetLearnerAndWinners.flatMap(_._2)
      val flat2 = datasetLearnerAndLosers.flatMap(_._2)
      ss foreach { st =>
         val topCount = flat.count(_ == st)
         val botCount = flat2.count(_ == st)
         println(s"${st.padTo(10, ' ')}:\t$topCount\t1st places;\t\t$botCount\tlast places")
      }
      println(s"------------------------------")
      println(s"")
      println(s"")
      println(s"")

      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(ss.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 100
      //      tbs foreach { case res1 =>
      //        StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ss.toVector.map(_.toString), "nomeTab", measure.toString)
      //      }
   }

}
