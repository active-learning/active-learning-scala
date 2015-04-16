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
import ml.classifiers.{RF, NinteraELM, SVMLibRBF}
import util.{Stat, StatTests}

object fried_tabela_strats_datasets extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedALCKappatex"
   val measure = ALCBalancedAcc
   val risco = false
   run()

   override def run() = {
      super.run()
      val caption = language match {
         case "pt" => s"Um contra um (LEA). Medida: $measure. \\textit{Legenda na Tabela \\ref{tab:friedClassif}.}"
         case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
      }
      val strats = stratsTex("all")
      for (le <- learners(learnersStr)) {
         val (sls, res0) = (for {
            dataset <- datasets
         //               .filter { d =>
         //               val ds = Ds(d, readOnly = true)
         //               ds.open()
         //               val r = ds.nclasses
         //               ds.close()
         //               //               r == 2
         //               true
         //            }.par
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val (ti, th, tf, tpass) = ranges(ds)
            val (sts, sres) = (for {
               s0 <- strats
            } yield {
               val s = s0(le)
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield try {
                     measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f)))
                  } catch {
                     case e: Throwable => error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f))
                  }
               s.abr -> (if (!risco) Stat.media_desvioPadrao(vs.toVector) else Stat.media_desvioPadrao(vs.toVector)._2 -> NA)
            }).unzip
            ds.close()
            sts -> (renomeia(ds) -> sres)
         }).unzip
         val sl = sls.head

         val sortedFiltered = res0.filter(!_._2.contains(NA, NA)).toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         sorted foreach (x => println(x._2.map(_._1).mkString(" ")))

         val arq = "/home/davi/wcs/tese/stratstab" + le.abr + ".tex"
         print(arq + "   ")
         val fw = new PrintWriter(arq, "ISO-8859-1")
         sorted.grouped(32).zipWithIndex.foreach { case (res1, i) =>
            fw.write(StatTests.extensiveTable2("Comparação de estratégias com aprendiz (se aplicável) e classificador " + le.abr +
               ". \\textit{Média e desvio padrão da ALC da acurácia balanceada. " +
               "Maior e menor média de cada base estão em \\textcolor{blue}{\\textbf{negrito azul}} e \\textcolor{red}{\\textbf{negrito vermelho}} respectivamente. " +
               "Maiores médias isoladas estão sublinhadas. " +
               "Os melhores valores de desvio padrão estão em \\textcolor{darkgreen}{verde}. Apenas negrito indica segundo melhor valor.}", true, 100, res1, sl.toVector, s"strats${i}a" + le.abr, "ALC para " + le.abr, 7))
            //            if (!redux)
            fw.write(StatTests.extensiveTable2("Comparação de estratégias com aprendiz (se aplicável) e classificador " + le.abr +
               ". \\textit{Média e desvio padrão da ALC da acurácia balanceada. " +
               "Maior e menor média de cada base estão em \\textcolor{blue}{\\textbf{negrito azul}} e \\textcolor{red}{\\textbf{negrito vermelho}} respectivamente. " +
               "Maiores médias isoladas estão sublinhadas. " +
               "Os melhores valores de desvio padrão estão em \\textcolor{darkgreen}{verde}. Apenas negrito indica segundo melhor valor.}", false, 100, res1, sl.toVector, s"strats${i}b" + le.abr, "ALC para " + le.abr, 7))
         }
         fw.close()

         println(s"${le.abr}:\t\t${sortedFiltered.size} datasets completos")
         val pairs = if (!risco) StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
         else StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
         val arq2 = "/home/davi/wcs/tese/stratsfried" + le.abr + (if (risco) "Risco" else "") + ".tex"
         println(s"")
         println(arq2)
         val fw2 = new PrintWriter(arq2, "ISO-8859-1")
         fw2.write(StatTests.pairTable(pairs, "stratsfried" + le.abr + (if (risco) "Risco" else ""), 2, caption.replace("LEA", le.abr)))
         fw2.close()
      }
   }

}
