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

import java.io.PrintWriter

import clean.lib._
import util.{Stat, StatTests}

object friedALCKappa extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedALCKappatex"
   val measure = ALCKappa
   val fried = true
   val risco = false
   run()

   override def run() = {
      super.run()
      for (le <- learners(learnersStr).par) {
         val strats = if (le.abr == "svm") {
            if (fried) stratsForTreeRedux() else stratsForTree()
         } else {
            if (fried) stratsForTreeSemSVMRedux else stratsForTreeSemSVM
         }
         val sl = strats.map(_.abr)
         val res0 = for {
            dataset <- datasets
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val (ti, tf) = maxRange(ds, 2, 200)
            val sres = for {
               s <- strats
            } yield {
               //               val le = if (s.id >= 17 && s.id <= 21 || s.id == 968 || s.id == 969) s.learner else l
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield
                  try {
                     measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(NA)
                  } catch {
                     case e: Throwable => NA
                  }

               if (vs.contains(NA)) throw new Error("NA")
               if (!risco) Stat.media_desvioPadrao(vs.toVector) else (vs.min, NA)
            }
            ds.close()
            //         (ds.dataset + l.toString.take(3)) -> sres
            renomeia(ds) -> sres
         }

         val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         if (!fried) {
            val fw = new PrintWriter("/home/davi/wcs/tese/stratsALCKappa" + le.abr + ".tex", "ISO-8859-1")
            sorted.grouped(32).zipWithIndex.foreach { case (res1, i) =>
               fw.write(StatTests.extensiveTable2(100, res1.toSeq.map(x => x._1 -> x._2.take(11)), sl.take(11).toVector.map(_.toString), s"stratsALCKappa${i}a" + le.abr, "ALCKappa para " + le.abr, 7))
               fw.write(StatTests.extensiveTable2(100, res1.toSeq.map(x => x._1 -> x._2.drop(11)), sl.drop(11).toVector.map(_.toString), s"stratsALCKappa${i}b" + le.abr, "ALCKappa para " + le.abr, 7))
            }
            fw.close()
         } else {
            val res = sorted.filter(!_._2.contains(NA, NA))
            val pairs = if (!risco) StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
            else StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
            val fw = new PrintWriter("/home/davi/wcs/tese/stratsALCKappaFried" + le.abr + (if (risco) "Risco" else "") + ".tex", "ISO-8859-1")
            fw.write(StatTests.pairTable(pairs, "stratsALCKappaFried" + le.abr + (if (risco) "Risco" else ""), "ALCKappa para " + le.abr))
            fw.close()
            println(s"${res.size} datasets completos")
         }
      }
   }
}
