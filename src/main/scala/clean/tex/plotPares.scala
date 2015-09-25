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

import al.strategies.{MetaLearnerBest, Strategy, MetaLearner}
import app.db.entities.Dataset
import clean.lib._
import ml.classifiers.{Learner, NoLearner}
import util.Stat

object plotPares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah")
  val context = "plotPares"
  //   val tipoSumariz = "mediana"
  val measure = Kappa
  val tipoSumariz = "media"
  val ls = (ds: Ds, st: Strategy) => (Seq("PCTr-a", "PCT", "RFw1000", "5NN", "chu", "defr-a", "maj", "C4.55", "rndr-a") map MetaLearner(ds, st)) ++ Seq(MetaLearnerBest(ds, st)) ++ learners(learnersStr)
  val strats = stratsTexForGraficoComplexo(dist)
  run()

  override def run() = {
    super.run()
    lazy val dss = datasets.filter { d =>
      val ds = Ds(d, readOnly = true)
      ds.open()
      val U = ds.poolSize.toInt
      ds.close()
      U > 200
    }
    val dsss = dss.take(599)
    val arq = s"/home/davi/wcs/tese/kappa$dist${tipoSumariz}Pares" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".plot"
    println(s"$arq")
    val algs = (for {s <- strats; l <- ls(null, null)} yield s(l).limp + "-" + l.limp).toVector
    val res0 = for {
      dataset <- dsss
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"$ds")
        ds.open()

        val sres = for {
          s0 <- strats.toList
          le <- ls(ds, s0(NoLearner()))
        } yield {
            val s = s0(le)
            val vs00 = for {
              r <- 0 until runs
              f <- 0 until folds
            } yield {
                measure(ds, s, le, r, f)(-2).readAll(ds).getOrElse {
                  //pelo que entendi -2 poderia ser -3456,... (19/9/15)
                  println((ds, s, le, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
                }
              }
            val sizes = vs00.map(_.size)
            val minsiz = sizes.min
            val vs0 = vs00.map(_.take(minsiz))
            if (minsiz != vs00.map(_.size).max) println(s"$dataset $s " + vs0.map(_.size).min + " " + vs0.map(_.size).max)
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

    val porLea = algs.zip(plot.transpose).groupBy(_._1.replace("-a", "+a").split("-").last).toList
    val bla = algs.zip(plot.transpose).map(_._1.replace("-a", "+a").split("-").last).toList
    println(s"${bla} <- bla")

    val minMaxBest = porLea map { case (lea, curvas) =>
      val min = "min" + lea -> curvas.map(_._2).transpose.map(_.min)
      val max = "max" + lea -> curvas.map(_._2).transpose.map(_.max)
      val best = curvas.minBy(_._2.sum)
      val resto = curvas.diff(Seq(best)).sortBy(_._2.sum)
      List(min, max, best) ++ resto
    }
    val (algs2, plot3) = minMaxBest.flatten.sortBy(_._2.sum).unzip
    val plot2 = plot3.transpose
    val fw = new PrintWriter(arq, "ISO-8859-1")
    fw.write("budget " + algs2.mkString(" ") + "\n")
    plot2.zipWithIndex foreach { case (re, i) =>
      fw.write((i + 10) + " " + re.map(_ / dss.size).mkString(" ") + "\n")
    }
    fw.close()
    println(s"$arq")

    algs2.zip(plot2.transpose).filter(x => !x._1.contains("max") && !x._1.contains("min")).foreach { case (alg, plo) =>
      println(s"${(100 * plo.map(_ / dsss.size).sum / plo.size).round / 100d} & $alg \\\\")
    }
  }
}
