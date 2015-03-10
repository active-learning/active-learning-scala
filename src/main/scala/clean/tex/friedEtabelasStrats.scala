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

object friedEtabelasStrats extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedALCKappatex"
   val measure = ALCKappa
   val risco = false
   val redux = true
   run()

   override def run() = {
      super.run()
      val caption = language match {
         case "pt" => s"Um contra um (LEA). Medida: $measure. \\textit{Legenda na Tabela \\ref{tab:friedClassif}.}"
         case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
      }
      for (le <- learners(learnersStr)) {
         val strats = if (redux) stratsForTreeRedux() else stratsForTree()
         val sl = le match {
            case _:SVMLibRBF => strats.dropRight(2)
            case _:NinteraELM => strats.dropRight(4) ++ strats.takeRight(2).dropRight(1)
            case _:RF => strats.dropRight(4) ++ strats.takeRight(1)
            case _ => strats.dropRight(4)
         }
         println(s"${le.abr} ${sl.size} ${sl.map(_.abr)} ")
         val res0 = for {
            dataset <- datasets.par
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val (ti, th, tf, tpass) = ranges(ds)
            val sres = (for {
               s <- sl
            } yield {
                  if (Global.gnosticasComLearnerInterno.contains(s.id) && s.learner.id != le.id) None
                  else Some({
                     val vs = for {
                        r <- 0 until runs
                        f <- 0 until folds
                     } yield
                        try {
                           measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(NA)
                        } catch {
                           case e: Throwable =>
                              println("NA:" +(ds, s.abr, le, r, f))
                              NA //sys.exit(1)
                        }
                     if (vs.contains(NA)) (NA, NA)
                     else if (!risco) Stat.media_desvioPadrao(vs.toVector) else (vs.min, NA)
                     //         (ds.dataset + l.toString.take(3)) -> sres
                  })
            }).flatten
            ds.close()
            renomeia(ds) -> sres
         }

         val sortedFiltered = res0.filter(!_._2.contains(NA, NA)).toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
         val fw = new PrintWriter("/home/davi/wcs/tese/stratsALCKappa" + le.abr + (if (redux) "Redux" else "") + ".tex", "ISO-8859-1")
         sorted.grouped(32).zipWithIndex.foreach { case (res1, i) =>
            fw.write(StatTests.extensiveTable2(true, 100, res1.toSeq.map(x => x._1 -> x._2), sl.toVector.map(_.abr), s"stratsALCKappa${i}a" + le.abr + (if (redux) "Redux" else ""), "ALCKappa para " + le.abr, 7))
            //            if (!redux)
            fw.write(StatTests.extensiveTable2(false, 100, res1.toSeq.map(x => x._1 -> x._2), sl.toVector.map(_.abr), s"stratsALCKappa${i}b" + le.abr + (if (redux) "Redux" else ""), "ALCKappa para " + le.abr, 7))
         }
         fw.close()
         println(s"${le.abr}:\t\t${sortedFiltered.size} datasets completos")
         //         val pairs = if (!risco) StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
         //         else StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
         //         val fw2 = new PrintWriter("/home/davi/wcs/tese/stratsALCKappaFried" + le.abr + (if (risco) "Risco" else "") + (if (redux) "Redux" else "") + ".tex", "ISO-8859-1")
         //         fw2.write(StatTests.pairTable(pairs, "stratsALCKappaFried" + le.abr + (if (risco) "Risco" else "") + (if (redux) "Redux" else ""), 2, caption.replace("LEA", le.abr)))
         //         fw2.close()
      }
   }
}
