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
import util.Stat

object plotKappa extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah")
   val context = "plotKappa"
   //   val tipoLearner = "best"
   val tipoLearner = "all"
   //      val tipo="mediano"
   //   val tipoSumariz = "mediana"
   val tipoSumariz = "media"
   val strats = stratsTex(dist)
   run()

   override def run() = {
      super.run()
      val arq = s"/home/davi/wcs/tese/kappa$dist$tipoSumariz$tipoLearner" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".plot"
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
      val (sls0, res9) = (for {
         dataset <- dss.take(1000)
         le0 <- ls2.par
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


         val (sls, sres) = (for {
            s0 <- strats
         } yield {
            val s = s0(le)
            val vs00 = try {
               for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield Kappa(ds, s, le, r, f)(0).readAll(ds)
            } catch {
               case _: Throwable => println(s"NA: ${(ds, s, le.abr)}")
                  Seq(None)
            }
            s -> (if (vs00.contains(None)) {
               println(s"NA: ${(ds, s, le.abr)}")
               None
            } else Some({
               val sizes = vs00.flatten.map(_.size)
               val minsiz = sizes.min
               val vs0 = vs00.flatten.map(_.take(minsiz))
               if (vs0.minBy(_.size).size != vs0.maxBy(_.size).size || minsiz != sizes.max) println(s"$dataset $s $le " + sizes.min + " " + sizes.max)
               val ts = vs0.transpose.map { v =>
                  if (porRisco) Stat.media_desvioPadrao(v.toVector)._2 * (if (porRank) -1 else 1)
                  else Stat.media_desvioPadrao(v.toVector)._1
               }
               val fst = ts.head
               ts.reverse.padTo(200, fst).reverse.toList
            }))
         }).unzip
         sls -> (if (sres.contains(None)) None
         else {
            ds.close()
            val sresf = sres.flatten
            lazy val rank = sresf.transpose map ranqueia
            val tmp = if (porRank) rank else sresf.transpose
            Some(tmp)
         })
      }).unzip
      val sls = sls0.head
      val res0 = res9.flatten
      val plot0 = res0ToPlot0(res0.toList, tipoSumariz)

      val plot = plot0.toList.transpose.map { x =>
         x.sliding(20).map(y => y.sum / y.size).toList
      }.transpose

      val fw = new PrintWriter(arq, "ISO-8859-1")
      fw.write("budget " + sls.map(_.limp).mkString(" ") + "\n")
      plot.zipWithIndex foreach { case (re, i) =>
         fw.write((i + 10) + " " + re.map(_ / (ls2.size * dss.size)).mkString(" ") + "\n")
      }
      fw.close()
      println(s"$arq " + (res0.size / ls2.size.toDouble) + " datasets completos.")
   }
}
