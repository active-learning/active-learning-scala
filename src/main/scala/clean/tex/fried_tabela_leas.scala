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

object fried_tabela_leas extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedLearnerstex"
   val measure = Kappa
   run()

   override def run() = {
      super.run()
      val caption = language match {
         case "pt" => s"\\textbf{Um contra um}. Medida: $measure. \\textit{Cada asterisco/cruz/ponto indica quando o algoritmo na linha tem melhor desempenho que o algoritmo na coluna com intervalo de confiança de 0.99/0.95/0.90.}"
         case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
      }
      val ls0 = learners(learnersStr).sortBy(_.abr).toVector
      val ls = ls0
      val res0 = for {
         dataset <- datasets
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val lres = for {
            l <- ls
         } yield {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               try {
                  measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse({
                     println("passiva NA:" +(ds, Passive(Seq()).abr, l, r, f))
                     NA
                  })
               } catch {
                  case e: Throwable => println("passiva pid NA:" +(ds, Passive(Seq()).abr, l, r, f))
                     NA
               }
            }
            if (vs.contains(NA)) (NA, NA)
            else Stat.media_desvioPadrao(vs.toVector)
         }
         ds.close()
          renomeia(ds.dataset) -> lres.map(_._1 -> NA)
      }
      res0 foreach (x => println(x._2.map(_._1).mkString(" ")))

      val sorted = res0.toList.filter(!_._2.contains(NA, NA)).sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)

     val fw = new PrintWriter("/home/davi/wcs/tese/classifsTab.tex") //, "ISO-8859-1")
     sorted.grouped(44).zipWithIndex foreach { case (res1, i) =>
         fw.write(StatTests.extensiveTable2("", true, 100, res1.toSeq, ls.map(_.abr), "tab:balaccClassif" + i, measure.toString, 5))
      }
      fw.close()
      //
      val pairs = StatTests.friedmanNemenyi(sorted.map(x => x._1 -> x._2.map(_._1)), ls.map(_.abr))
     val fw7 = new PrintWriter("/home/davi/wcs/tese/classifsFried.tex") //, "ISO-8859-1")
      fw7.write(StatTests.pairTable(pairs, "tab:friedClassif", 2, caption))
      fw7.close()
      println(s"${sorted.size} datasets completos")

      println(s"1os lugares")
      val vics = sorted.map(x => ls.zip(x._2.map(_._1)).maxBy(_._2)._1.abr)
      println(vics.groupBy(x => x).map(x => x._1 -> x._2.size).toList.sortBy(_._2).reverse.map(x => x._1 + " & " + x._2 + " \\\\").mkString("\n"))

      //      println(s"")
      //      println(s"2os lugares")
      //      val vics2 = sorted.map(x => ls.zip(x._2.map(_._1)).sortBy(_._2).reverse.tail.head._1.abr)
      //      println(vics2.groupBy(x => x).map(x=> x._1 -> x._2.size).toList.sortBy(_._2).reverse.mkString("\n"))

      //      println(s"")
      //      println(s"ambos")
      //      val vicsa = vics ++ vics2
      //   println(vicsa.groupBy(x => x).map(x=> x._1 -> x._2.size).toList.sortBy(_._2).reverse.mkString("\n"))
   }
}
