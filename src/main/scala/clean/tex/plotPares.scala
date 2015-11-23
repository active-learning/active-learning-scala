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

import al.strategies.RandomSampling
import clean.lib._
import ml.classifiers.{Learner, NoLearner}
import util.Stat

object plotPares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah", "sóRnd:rnd")
  val context = "plotPares"
  //   val tipoSumariz = "mediana"
  val measure = Kappa
  val tipoSumariz = "media"
  //  val ls = (ds: Ds, st: Strategy) => (Seq("SVM", "PCTr-a", "PCT", "RFw1000", "5NN", "chu", "defr-a", "maj", "C4.55", "rndr-a") map MetaLearner(ds, st)) ++ Seq(MetaLearnerBest(ds, st)) ++ learners(learnersStr)
  //  val ls = (ds: Ds, st: Strategy) => (Seq("PCTr-a", "PCT", "RFw1000", "chu", "defr-a", "maj", "rndr-a") map MetaLearner(ds, st)) ++ Seq(MetaLearnerBest(ds, st)) ++ learners(learnersStr)
  //  def ls (ds: Ds, st: Strategy,r:Int,f:Int) = learners(learnersStr, Seq(), -1, ds, st,r,f)
  val strats = if (sohRnd) Seq((l: Learner) => RandomSampling(Seq())) else stratsTexForGraficoSimples
  //  val strats = stratsTexForGraficoSimplesIlustra
  run()

  override def run() = {
    super.run()
    val arq = s"/home/davi/wcs/tese/$learnerStr-" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".tex"
    //    val arq = s"/home/davi/wcs/tese/ilustra$learnerStr-" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".tex"
    println(s"$arq")
    val algs = (for {s <- strats; l <- learners(learnersStr)} yield s(l).limp + "-" + l.limp).toVector
    val res0 = for {
      dataset <- datasets
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"$ds")
        ds.open()
        val sres = for {
          s0 <- strats.toList
          lestr <- learnersStr
        } yield {
            val vs00 = for {
              r <- 0 until runs
              f <- 0 until folds
            } yield {
                val le = str2learner(Seq(), -1, ds, s0(NoLearner()), learnersStr.filter(!_.startsWith("meta")), r, f)(lestr)
                val s = s0(le)
                measure(ds, s, le, r, f)(-2).readAll99(ds).getOrElse {
                  //pelo que entendi -2 poderia ser -3456,... (19/9/15)
                  println((ds, s, le, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
                }
              }
            val sizes = vs00.map(_.size)
            val minsiz = sizes.min
            val vs0 = vs00.map(_.take(minsiz))
            //            if (minsiz != vs00.map(_.size).max) println(s"$dataset $s " + vs0.map(_.size).min + " " + vs0.map(_.size).max)
            val ts = vs0.transpose.map { v =>
              if (porRisco) Stat.media_desvioPadrao(v.toVector)._2 * (if (porRank) -1 else 1)
              else Stat.media_desvioPadrao(v.toVector)._1
            }
            val fst = ts.head
            ts.reverse.padTo(100, fst).reverse.toList
          }

        ds.close()
        lazy val rank = sres.transpose map ranqueia
        val tmp = if (porRank) rank else sres.transpose
        tmp
      }
    val plot0 = res0ToPlot0(res0.toList, tipoSumariz)
    val plot = plot0.toList.transpose.map { x =>
      x.sliding(5).map(y => y.sum / y.size).toList
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
    val fw = new PrintWriter(arq) //, "ISO-8859-1")
    fw.write("budget " + algs2.mkString(" ") + "\n")
    plot2.zipWithIndex foreach { case (re, i) =>
      fw.write((i + 2) + " " + re.map(_ / datasets.size).mkString(" ") + "\n")
    }
    fw.close()
    println(s"$arq")

    algs2.zip(plot2.transpose).filter(x => !x._1.contains("max") && !x._1.contains("min")).foreach { case (alg, plo) =>
      println(s"${(100 * plo.map(_ / datasets.size).sum / plo.size).round / 100d} & $alg \\\\")
    }
  }
}
