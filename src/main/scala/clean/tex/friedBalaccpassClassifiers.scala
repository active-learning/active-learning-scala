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

object friedBalaccpassClassifiers extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedBalaccpassClassifierstex"
   //         val measure = Kappa
   val measure = BalancedAcc
   run()

   override def run() = {
      super.run()
      val ls = learners(learnersStr).map(_.abr).toVector
      val res0 = for {
         dataset <- datasets
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val lres = for {
            l <- learners(learnersStr).sortBy(_.abr)
         } yield {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               try {
                  measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(throw new Error("passiva não encontrada"))
               } catch {
                  case e: Throwable => println(s"pid não encontrado")
                     NA
               }
            }
            Stat.media_desvioPadrao(vs.toVector)
         }
         ds.close()
         renomeia(ds) -> lres.map(_._1 -> NA)
      }
      val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)

      var fw = new PrintWriter("/home/davi/wcs/tese/classifsTab.tex", "ISO-8859-1")
      sorted.grouped(33).zipWithIndex foreach { case (res1, i) =>
         fw.write(StatTests.extensiveTable2(take11 = true, 100, res1.toSeq, ls, "tab:balaccClassif" + i, measure.toString, 7))
      }
      fw.close()

      val pairs = StatTests.friedmanNemenyi(sorted.map(x => x._1 -> x._2.map(_._1)), ls)

      fw = new PrintWriter("/home/davi/wcs/tese/classifsFried.tex", "ISO-8859-1")
      fw.write(StatTests.pairTable(pairs, "tab:friedClassif", "acurácia balanceada"))
      fw.close()
   }
}
