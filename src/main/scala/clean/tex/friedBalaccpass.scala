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

import al.strategies.Passive
import clean.lib._
import util.{Stat, StatTests}

object friedBalaccpass extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedPasstex"
   val measure = BalancedAcc
   val risco = false
   run()

   override def run() = {
      super.run()
      for (le <- learners(learnersStr).par) {
         val strats = Passive(Seq()) +: (if (le.abr == "svm") stratsForTree() else stratsForTreeSemSVM)
         val sl = strats.map(_.abr)
         val res0 = for {
            dataset <- datasets
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val t = ranges(ds, 2, 200).head._2 //metade de U, mas limitado por 200
            val sres = for {
                  s <- strats
               } yield {
                  val vs = for {
                     r <- 0 until runs
                     f <- 0 until folds
                  } yield s match {
                        case Passive(Seq(), false) => measure(ds, Passive(Seq()), le, r, f)(-1).read(ds).getOrElse(ds.quit("passiva não encontrada"))
                        case _ => measure(ds, s, le, r, f)(t).read(ds).getOrElse(NA)
                     }

                  if (vs.contains(NA)) throw new Error("NA")
                  if (!risco) Stat.media_desvioPadrao(vs.toVector) else (vs.min, NA)
               }
            ds.close()
            //         (ds.dataset + l.toString.take(3)) -> sres
            renomeia(ds) -> sres
         }

         val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         val fw = new PrintWriter("/home/davi/wcs/tese/stratsBalAcc" + le.abr + ".tex", "ISO-8859-1")
         sorted.grouped(32).zipWithIndex.foreach { case (res1, i) =>
            fw.write(StatTests.extensiveTable2(100, res1.toSeq.map(x => x._1 -> x._2.take(11)), sl.take(11).toVector.map(_.toString), s"stratsBalAcc${i}a" + le.abr, "acurácia balanceada para " + le.abr, 7))
            fw.write(StatTests.extensiveTable2(100, res1.toSeq.map(x => x._1 -> x._2.drop(11)), sl.drop(11).toVector.map(_.toString), s"stratsBalAcc${i}b" + le.abr, "acurácia balanceada para " + le.abr, 7))
         }
         fw.close()

         //         val res = sorted.filter(!_._2.contains(NA, NA))
         //         val pairs = if (!risco) StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
         //         else StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
         //         fw = new PrintWriter("/home/davi/wcs/tese/stratsBalAccFried" + learnersStr.mkString("") + (if (risco) "Risco" else "") + ".tex", "ISO-8859-1")
         //         fw.write(StatTests.pairTable(pairs, "stratsBalAccFried", "acurácia balanceada"))
         //         fw.close()
         //         println(s"${res.size} datasets completos")
      }
   }
}
