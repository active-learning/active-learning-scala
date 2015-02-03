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

object plotKappabest extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "plotKappabest"
   val measure = Kappa
   val redux = true
   //   val risco = true
   val risco = false
   val porRank = true
   //   val porRank = false
   run()

   override def run() = {
      super.run()
      val strats = if (redux) stratsForTreeRedux() else stratsForTree()
      val sl = strats.map(_.abr)
      val dss = datasets.filter { d =>
         val ds = Ds(d, readOnly = true)
         ds.open()
         val U = ds.poolSize.toInt
         ds.close()
         U > 200
      }
      val res0 = for {
         dataset <- dss.par
      } yield {
         val ds = Ds(dataset, readOnly = true)
         //         println(s"$ds")
         ds.open()
         val le = learners(learnersStr).map { l =>
            val vs = for (r <- 0 until runs; f <- 0 until folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
            l -> Stat.media_desvioPadrao(vs.toVector)._1
         }.maxBy(_._2)._1
         val (ti, th, tf, tpass) = ranges(ds)
         val sres = for {
            s <- strats
         } yield {
            val ts = ti to tf map { t =>
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield measure(ds, s, le, r, f)(t).read(ds).getOrElse(throw new Error("NA"))
               Stat.media_desvioPadrao(vs.toVector)._1
            }
            val fst = ts.head
            ts.reverse.padTo(200, fst).reverse
         }
         ds.close()
         lazy val rank = sres.transpose.map { vsAtT =>
            val idxERank = vsAtT.zipWithIndex.sortBy(_._1).reverse.zipWithIndex
            val idxEAvrRank = idxERank.groupBy { case ((v, idx), ra) => ff(1000)(v)}.toList.map { case (k, g) =>
               val avrRa = g.map { case ((v, idx), ra) => ra}.sum.toDouble / g.size
               g.map { case ((v, idx), ra) => idx -> avrRa}
            }.flatten
            idxEAvrRank.sortBy(_._1).map(_._2)
         }
         if (porRank) rank else sres.transpose
      }
      val plot = res0.foldLeft(Seq.fill(sl.size * 200)(0d)) { (l, m) =>
         m.flatten.zip(l).map(x => x._1 + x._2)
      }.grouped(sl.size)
      println(sl.mkString(" "))
      plot foreach { re =>
         println(re.map(_ / dss.size).mkString(" "))
      }

      //
      //      val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
      //      val fw = new PrintWriter("/home/davi/wcs/tese/stratsALCKappabest" + (if (redux) "Redux" else "") + ".tex", "ISO-8859-1")
      //      sorted.grouped(32).zipWithIndex.foreach { case (res1, i) =>
      //         fw.write(StatTests.extensiveTable2(true, 100, res1.toSeq.map(x => x._1 -> x._2), sl.toVector.map(_.toString), s"stratsALCKappa${i}best" + (if (redux) "Redux" else "") + "a", "ALCKappa para melhor aprendiz", 7))
      //         fw.write(StatTests.extensiveTable2(false, 100, res1.toSeq.map(x => x._1 -> x._2), sl.toVector.map(_.toString), s"stratsALCKappa${i}best" + (if (redux) "Redux" else "") + "b", "ALCKappa para melhor aprendiz", 7))
      //      }
      //      fw.close()
      //
      //      val res = sorted.filter(!_._2.contains(NA, NA))
      //      val pairs = if (!risco) StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
      //      else StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
      //      val fw2 = new PrintWriter("/home/davi/wcs/tese/stratsALCKappaFriedbest" + (if (risco) "Risco" else "") + (if (redux) "Redux" else "") + ".tex", "ISO-8859-1")
      //      fw2.write(StatTests.pairTable(pairs, "stratsALCKappaFriedbest" + (if (risco) "Risco" else "") + (if (redux) "Redux" else ""), "ALCKappa para melhor aprendiz"))
      //      fw2.close()
      //      println(s"${res.size} datasets completos")
   }
}
