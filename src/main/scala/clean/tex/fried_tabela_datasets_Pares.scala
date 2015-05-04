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
import ml.classifiers._
import util.{Stat, StatTests}

object fried_tabela_datasets_Pares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedParestex"
   val measure = Kappa
   val risco = false
   run()

   override def run() = {
      super.run()
      val caption = language match {
         case "pt" => s"Pares um contra um (STR). Medida: $measure. \\textit{Legenda na Tabela \\ref{tab:friedClassif}.}"
         case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
      }
      val ls = learners(learnersStr)
      val strats = (for {l <- ls; s <- stratsTexRedux("all").map(_(l))} yield s).distinct

      val (sls, res0) = (for {
         dataset <- datasets.toList.filter { dataset =>
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val r = ds.poolSize >= 200
            ds.close()
            r
         }
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val (sts, sres) = (for {
            s <- strats
         } yield {
            val vs = (for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
               classif.limpa -> measure(ds, s, classif, r, f)(-2).read(ds).getOrElse {
                  println((ds, s, s.learner, classif, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
               }
            }) map (_._2)
            s.abr -> (if (!risco) Stat.media_desvioPadrao(vs.toVector) else Stat.media_desvioPadrao(vs.toVector)._2 -> NA)
         }).unzip
         ds.close()
         sts -> (renomeia(ds) -> sres)
      }).unzip
      val sl = sls.head

      val sortedFiltered = res0.filter(!_._2.contains(NA, NA)).toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      sorted foreach (x => println(x._2.map(_._1).mkString(" ")))

      println(s"\t${sortedFiltered.size} datasets completos")
      val pairs = if (!risco) StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
      else StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
      val arq2 = s"/home/davi/wcs/tese/stratsfriedPares$measure" + (if (risco) "Risco" else "") + ".tex"
      println(s"")
      println(arq2)
      val fw2 = new PrintWriter(arq2, "ISO-8859-1")
      fw2.write(StatTests.pairTable(pairs, "stratsfriedpares" + (if (risco) "Risco" else ""), 2, caption))
      fw2.close()
   }
}
