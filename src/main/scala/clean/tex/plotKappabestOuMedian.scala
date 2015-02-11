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

object plotKappabestOuMedian extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "plotKappabest"
   val porRank = !true
   //   val tipo = "best"
   val tipoLearner = "all"
   //      val tipo="mediano"
   //   val tipoSumariz = "mediana"
   val tipoSumariz = "media"
   val redux = porRank
   val strats = if (redux) stratsForTreeRedux() else stratsForTree()
   val sl = strats.map(_.abr)
   run()

   def res0ToPlot0(res0: List[Seq[Seq[Double]]]) = tipoSumariz match {
      case "media" => res0.foldLeft(Seq.fill(sl.size * 200)(0d)) { (l, m) =>
         m.flatten.zip(l).map(x => x._1 + x._2)
      }.grouped(sl.size).toList.map(_.toList)
      case "mediana" => res0.map(_.flatten).transpose.map(x => x.sorted.toList(x.size / 2)).grouped(sl.size).toList.map(_.toList)
   }

   override def run() = {
      super.run()
      val arq = s"/home/davi/wcs/tese/kappa$tipoSumariz$tipoLearner" + (if (redux) "Redux" else "") + (if (porRank) "Rank" else "") + ".plot"
      println(s"$arq")
      val ls = learners(learnersStr)
      val ls2 = tipoLearner match {
         case "best" | "mediano" => Seq(NoLearner())
         case "all" => ls
      }
      val dss = datasets.filter { d =>
         val ds = Ds(d, readOnly = true)
         ds.open()
         val U = ds.poolSize.toInt
         ds.close()
         U > 200
      }
      val res0 = for {
         dataset <- dss.take(1000).par
         le0 <- ls2
      } yield {
         val ds = Ds(dataset, readOnly = true)
         println(s"$ds")
         ds.open()
         val le = tipoLearner match {
            case "mediano" => ls.map { l =>
               val vs = for (r <- 0 until runs; f <- 0 until folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
               l -> Stat.media_desvioPadrao(vs.toVector)._1
            }.sortBy(_._2).apply(ls.size / 2)._1
            case "best" => ls.map { l =>
               val vs = for (r <- 0 until runs; f <- 0 until folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
               l -> Stat.media_desvioPadrao(vs.toVector)._1
            }.maxBy(_._2)._1
            case "all" => le0
         }

         val (ti, th, tf, tpass) = ranges(ds)
         val sres = for {
            s <- strats
         } yield {
            val ts = ti to tf map { t =>
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield Kappa(ds, s, le, r, f)(t).read(ds).getOrElse(throw new Error("NA"))
               Stat.media_desvioPadrao(vs.toVector)._1
            }
            val fst = ts.head
            ts.reverse.padTo(200, fst).reverse.toList
         }
         ds.close()
         lazy val rank = sres.transpose.map { vsAtT =>
            val idxERank = vsAtT.zipWithIndex.sortBy(_._1).reverse.zipWithIndex
            val idxEAvrRank = idxERank.groupBy { case ((v, idx), ra) => ff(1000)(v)}.toList.map { case (k, g) =>
               val gsize = g.size
               val avrRa = g.map { case ((v, idx), ra) => ra}.sum.toDouble / gsize + 1 // +1 pra corrigir o índice zero
               g.map { case ((v, idx), ra) => idx -> avrRa}
            }.flatten
            idxEAvrRank.sortBy(_._1).map(_._2)
         }
         val tmp = if (porRank) rank else sres.transpose
         tmp
      }
      val plot0 = res0ToPlot0(res0.toList)

      val plot = plot0.toList.transpose.map { x =>
         x.sliding(10).map(y => y.sum / y.size).toList
      }.transpose

      val fw = new PrintWriter(arq, "ISO-8859-1")
      fw.write("budget " + sl.map(_.replace("}", "").replace("\\textbf{", "")).mkString(" ") + "\n")
      plot.zipWithIndex foreach { case (re, i) =>
         fw.write(i + " " + re.map(_ / (ls2.size * dss.size)).mkString(" ") + "\n")
      }
      fw.close()
      println(s"$arq")
   }
}
