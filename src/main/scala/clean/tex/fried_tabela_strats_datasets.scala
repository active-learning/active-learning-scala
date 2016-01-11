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

object fried_tabela_strats_datasets extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "friedALCtex"
  val measure = ALCKappa
  val risco = false
  run()

  override def run() = {
    super.run()
    val caption = language match {
      case "pt" => s"Um contra um (STR). Medida: $measure. \\textit{Legenda na Tabela \\ref{tab:friedClassif}.}"
      case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
    }
    val strats = stratsTexForGraficoComplexo
    for (le <- learners(learnersStr)) {
      val (sls, res0) = (for {dataset <- datasets} yield {
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
        sts.map(_.replace("Clu", "HS")) -> (renomeia(ds.dataset) -> sres)
      }).unzip
      val sl = sls.head

      val sortedFiltered = res0.filter(!_._2.contains(NA, NA)).sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      val sorted = res0.toList.zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      //      val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      sorted foreach (x => println(x._2.map(_._1).mkString(" ")))

      val arq = s"/home/davi/wcs/tese/images/stab" + le.abr
      //      val arq = s"/home/davi/wcs/artigos/revista-comparacao/tempstratstab$measure" + le.abr + ".tex"
      print(arq + "   ")

      sorted.grouped(45).zipWithIndex.foreach { case (res1, i) =>
        val fai = if (i == 0) "1-45" else "46-90"
        val fw = new PrintWriter(arq + i + ".tex") //, "ISO-8859-1")
        fw.write(StatTests.extensiveTable2("", take11 = true, 100, res1, sl.toVector, s"strats${i}a" + le.abr, "ALC para " + le.abr, 5, sohcore = true).dropRight(6))
        fw.close()

      }

      //      println(s"${le.abr}:\t\t${sortedFiltered.size} datasets completos")
      //      val pairs = if (!risco) StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
      //      else StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
      //      val arq2 = s"/home/davi/wcs/tese/images/stratsfried$measure" + le.abr + (if (risco) "Risco" else "") + ".tex"
      //      //      val arq2 = s"/home/davi/wcs/artigos/revista-comparacao/tempstratsfried$measure" + le.abr + (if (risco) "Risco" else "") + ".tex"
      //      println(s"")
      //      println(arq2)
      //      val fw2 = new PrintWriter(arq2) //, "ISO-8859-1")
      //      fw2.write(StatTests.pairTable(pairs, "stratsfried" + le.abr + (if (risco) "Risco" else ""), 2, caption.replace("LEA", le.abr)))
      //      fw2.close()
    }
  }

}
