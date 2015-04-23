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
import ml.classifiers.NoLearner
import util.{Stat, StatTests}

object plotPares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah")
   val context = "plotKappa2"
   //   val tipoSumariz = "mediana"
   val tipoSumariz = "media"
   val redux = true
   val strats = if (redux) dist match {
      case "euc" => stratsForTreeReduxEuc().dropRight(4)
      case "man" => stratsForTreeReduxMan().dropRight(4)
      case "mah" => stratsForTreeReduxMah().dropRight(4)
   } else stratsForTree()
   val ls = learners(learnersStr)
   val sl = strats.map(_.abr)
   run()

   override def run() = {
      super.run()
      val arq = s"/home/davi/wcs/tese/kappa$dist${tipoSumariz}Pares" + (if (redux) "Redux" else "") + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".plot"
      println(s"$arq")
      val algs = (for (s <- strats; le <- ls) yield s.limpa + le.limpa).toVector
      val dss = datasets.filter { d =>
         val ds = Ds(d, readOnly = true)
         ds.open()
         val U = ds.poolSize.toInt
         ds.close()
         U > 200
      }
      val res0 = for {
         dataset <- dss.take(1000).par
      } yield {
         val ds = Ds(dataset, readOnly = true)
         println(s"$ds")
         ds.open()

         val sres = for {
            s <- strats
            le <- ls
         } yield {
            val vs00 = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield BalancedAcc(ds, s, le, r, f)(0).readAll(ds).getOrElse(throw new Error(s"NA: ${(ds, s, le.abr, r, f)}"))
            val sizes = vs00.map(_.size)
            val minsiz = sizes.min
            val vs0 = vs00.map(_.take(minsiz))
            if (minsiz != vs00.map(_.size).max) println(s"$dataset $s $le " + vs0.map(_.size).min + " " + vs0.map(_.size).max)
            val ts = vs0.transpose.map { v =>
               if (porRisco) Stat.media_desvioPadrao(v.toVector)._2 * (if (porRank) -1 else 1)
               else Stat.media_desvioPadrao(v.toVector)._1
            }
            val fst = ts.head
            ts.reverse.padTo(200, fst).reverse.toList
         }

         ds.close()
         lazy val rank = sres.transpose map ranqueia
         val tmp = if (porRank) rank else sres.transpose
         tmp
      }
      val plot0 = res0ToPlot0(res0.toList, tipoSumariz)
      val plot = plot0.toList.transpose.map { x =>
         x.sliding(20).map(y => y.sum / y.size).toList
      }.transpose
      val n = 12
      println(s"Pega apenas $n mais importantes.")
      val (algs2, plot3) = algs.zip(plot.transpose).sortBy(_._2.min).take(n).unzip
      val plot2 = plot3.transpose

      val fw = new PrintWriter(arq, "ISO-8859-1")
      fw.write("budget " + algs2.mkString(" ") + "\n")
      plot2.zipWithIndex foreach { case (re, i) =>
         fw.write((i + 10) + " " + re.map(_ / dss.size).mkString(" ") + "\n")
      }
      fw.close()
      println(s"$arq")
   }
}
