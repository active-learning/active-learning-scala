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

import al.strategies.RandomSampling
import clean.lib._
import ml.classifiers.NoLearner
import util.Stat

object tabwinners extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  //, "porRisco:r", "dist:euc,man,mah")
  val context = "tabwinnerstex"
  val n = 1
  val risco = false
  val measure = ALCKappa
  val sts = stratsTexForGraficoSimples //.dropRight(1)
  run()

  override def run() = {
    super.run()
    if (risco) println(s" medindo risco!!!!")
    val ls = learners(learnersStr)
    val datasetLearnerAndBoth = for {
      dataset <- datasets.toList.par
      l <- ls
    } yield {
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        val (ti, th, tf0, tpass) = ranges(ds)
        val tf = 99
        val sres = for {
          s0 <- sts
          s = s0(l)
        } yield {
            val vs = for {
              r <- 0 until runs
              f <- 0 until folds
            } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                throw new Error((ds, s, l, r, f) + ": medida não encontrada")
              }
            if (risco) s.limp.takeWhile(x => x != ' ') -> Stat.media_desvioPadrao(vs.toVector)._2 * -1
            else s.limp.takeWhile(x => x != ' ') -> Stat.media_desvioPadrao(vs.toVector)._1
          }
        val rnd = sres.find(_._1 == RandomSampling(Seq()).limp).get._2
        val r = Some(ds.dataset + l.abr -> pegaMelhores(sres, n)(_._2).map(_._1),
          ds.dataset + l.abr -> pegaMelhores(sres, n)(-_._2).map(_._1),
          ds.dataset + l.abr -> sres.filter(_._2 < rnd).map(_._1))
        ds.close()
        r
      }

    val (datasetLearnerAndWinners, datasetLearnerAndLosers, pioresQueRnd) = datasetLearnerAndBoth.flatten.unzip3

    println(datasetLearnerAndWinners)
    println(s"${datasetLearnerAndBoth.size} tests.")
    println(s"--------$measure---------------")
    //      datasetLearnerAndWinners foreach println
    val flat = datasetLearnerAndWinners.flatMap(_._2)
    val flat2 = datasetLearnerAndLosers.flatMap(_._2)
    val flat3 = pioresQueRnd.flatMap(_._2)
    val algs = (for (s <- sts) yield s(NoLearner()).limp) map { st =>
      val topCount = flat.count(_ == st)
      val botCount = flat2.count(_ == st)
      val rndCount = flat3.count(_ == st)
      (st, topCount, rndCount, botCount)
    }
    val topseq = Seq(algs.map(_._2).sorted.reverse(1), algs.map(_._2).max, algs.map(_._2).min)
    val rndseq = Seq(algs.map(_._3).sorted.toList(2), algs.map(_._3).sorted.toList(1), algs.map(_._3).max)
    val botseq = Seq(algs.map(_._4).sorted.toList(1), algs.map(_._4).min, algs.map(_._4).max)
    //    val Rounds = ls.size * datasets.size
    def decorar(C: Int, S: Seq[Int]) = (C, S) match {
      case (0, _) => "-"
      case (_, Seq(_, C, _)) => s"\\bom{$C}"
      case (_, Seq(C, _, _)) => s"\\bomd{$C}"
      case (_, Seq(_, _, C)) => s"\\ruim{$C}"
      case _ => C.toString
    }

    println( """\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
\hline
             """)
    algs.sortBy(_._2).reverse foreach { case (st, topCount, rndCount, botCount) =>
      val top = decorar(topCount, topseq)
      val rn = decorar(rndCount, rndseq)
      val bot = decorar(botCount, botseq)
      println(s"${st.padTo(10, ' ')} & $top & $rn & $bot \\\\")
    }
    println(
      """\end{tabular}
      """.stripMargin)

    //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
    //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
    //      val tbs = res.map(x => x._1 -> x._2.padTo(ss.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 100
    //      tbs foreach { case res1 =>
    //        StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ss.toVector.map(_.toString), "nomeTab", measure.toString)
    //      }
    if (risco) println(s" medindo risco!!!!")
  }
}

/*
\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
\hline

EERent     & 275 & 186 & 122 \\
HTUmah     & 230 & 195 & 104 \\
Mar        & 210 & 231 & 192 \\
TUmah      & 206 & 219 & 128 \\
ATUmah     & 199 & 210 & 144 \\
SGmulti    & 175 & 246 & 131 \\
SVMbal     & 145 & 330 & 295 \\
QBCRFw     & 110 & 330 & 272 \\
Clu        & 105 & 228 & 143 \\
Rnd        & 84 & 564 & 195 \\
\end{tabular}


\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
\hline

EERent     & 267 & 192 & 126 \\
HTUmah     & 234 & 225 & 118 \\
ATUmah     & 208 & 232 & 149 \\
TUmah      & 193 & 245 & 140 \\
Mar        & 178 & 260 & 189 \\
SGmulti    & 162 & 268 & 146 \\
SVMbal     & 143 & 331 & 279 \\
Clu        & 134 & 234 & 119 \\
Rnd        & 105 & 564 & 186 \\
QBCRFw     & 104 & 343 & 274 \\
\end{tabular}

 */