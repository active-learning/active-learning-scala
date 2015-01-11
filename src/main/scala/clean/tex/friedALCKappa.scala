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

import al.strategies.Passive
import clean._
import clean.meta.RangeGenerator
import clean.res._
import util.{Stat, StatTests}

object friedALCKappa extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedALCKappatex"
   val measure = ALCKappa
   //   val measure = ALCBalancedAcc
   run()

   def ff(precision: Double)(x: Double) = (x * precision).round / precision

   override def run() = {
      super.run()
      val strats = stratsForTree()
      //      val strats = allStrats()
      val sl = strats.map(_.abr)

      val res0 = for {
         dataset <- datasets
         l <- learners(learnersStr).par
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         //         val (ti, tf) = maxRange(ds)
         //         ds.log("",30)
         val sres = for {
            s <- strats
         } yield {
            val le = if (s.id >= 17 && s.id <= 21 || s.id == 968 || s.id == 969) s.learner else l
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               s match {
                  case _ =>
                     val (ti, tf) = maxRange(ds, 2, 100) //<- verificar se põe 200
                     try {
                        measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(NA)
                     } catch {
                        case e: Throwable => NA
                     }
               }
            }

            //por media
            if (vs.contains(NA)) (NA, NA) else Stat.media_desvioPadrao(vs.toVector)

            //pela pior medida
            //            if (vs.contains(-2d)) (-2d, -2d) else (vs.min, -2d)

         }
         ds.close()
         (ds.dataset + l.toString.take(3)) -> sres
      }

      val res0sorted = res0.toList.sortBy(x => x._2.count(_._1 == -2d))

      println(s"")
      res0sorted.grouped(280).foreach { res1 =>
         StatTests.extensiveTable2(1000, res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), sl.toVector.map(_.toString), "nomeTab", measure.toString, 7)
      }

      println(s"")
      val res = res0sorted.filter(!_._2.contains(-2d, -2d))

      //por medida
      val pairs = StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(_._1)), sl.toVector)

      //por 1-desvio
      //            val res2 = res.map(x => x._1 -> x._2.map(1 - _._2).drop(1))
      //            val pairs = StatTests.friedmanNemenyi(res2, sl.toVector.drop(1))

      StatTests.pairTable(pairs, "tablename", "acc")
   }
}
