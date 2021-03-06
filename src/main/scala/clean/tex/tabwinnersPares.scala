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

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.Stat

object tabwinnersPares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "r:sóMar", "?", "?", "?", "?", "ini", "fim")
  val context = "tabwinnersPares"
  val n = 1
  val measure = ALCKappa
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    val strats = if (porRank) Seq((learner: Learner) => MarginFixo(learner, fakePool)) else stratsTexForGraficoComplexo

    val datasetLearnerAndBoth = for {
      dataset <- datasets.toList
    } yield {
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        lazy val (ti0, th0, tf0, tpass) = ranges(ds)
        val ti = ini match {
          case "ti" => ti0
          case "th" => th0 + 1
        }
        val tf = fim match {
          case "th" => th0
          case "tf" => tf0
        }

        val sres = for {s0 <- strats; classif <- ls} yield {
          val s = s0(classif)
          val (cs, vs) = (for {
            r <- 0 until runs
            f <- 0 until folds
          } yield {
              try {
                classif.limpa -> measure(ds, s, classif, r, f)(ti, tf).read(ds).getOrElse {
                  println((ds, s, s.learner, classif, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
                }
              } catch {
                case e: Throwable => println((ds, s, classif, r, f) + "\n" + e.getMessage)
                  sys.exit(0) //NA
              }
            }).unzip
          //            if (vs.contains(NA)) None else Some(s.limpa + cs.mkString(";") -> Stat.media_desvioPadrao(vs.toVector)._1)
          s.limp + classif.limp -> Stat.media_desvioPadrao(vs.toVector)._1
        }
        //        val rnd = sres.find(_._1 == RandomSampling(Seq()).limp).getOrElse("" -> 0d)._2
        val res = (ds.dataset -> pegaMelhores(sres, n)(_._2).map(_._1), ds.dataset -> pegaMelhores(sres, n)(-_._2).map(_._1))
        //          ds.dataset -> sres.filter(_._2 <= rnd).map(_._1).toList)
        //        val res = (ds.dataset -> pegaMelhores(sres, n)(_._2).map(_._1),
        //          ds.dataset -> pegaMelhores(sres, n)(-_._2).map(_._1),
        //          ds.dataset -> sres.filter(_._2 <= rnd).map(_._1).toList)
        ds.close()
        res
      }

    val (datasetLearnerAndWinners, datasetLearnerAndLosers) = datasetLearnerAndBoth.unzip
    //    val (datasetLearnerAndWinners, datasetLearnerAndLosers, pioresQueRnd) = datasetLearnerAndBoth.unzip3
    println(s"$n primeiros/últimos")
    println(s"${datasetLearnerAndBoth.size} tests.")
    println(s"--------$measure---------------")
    val flat = datasetLearnerAndWinners.flatMap(_._2)
    val flat2 = datasetLearnerAndLosers.flatMap(_._2)
    //    val flat3 = pioresQueRnd.flatMap(_._2)
    val strats0 = (for {l <- ls; s <- strats.map(x => x(l).limp + l.limp)} yield s).distinct
    val algs1 = strats0 map { st =>
      val topCount = flat.count(_ == st)
      val botCount = flat2.count(_ == st)
      //      val rndCount = flat3.count(_ == st)
      (st, topCount, botCount)
      //      (st, topCount, rndCount, botCount)
    }

    println( """\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{últimos\\lugares} \\
\hline
             """)
    //    println( """\begin{tabular}{lccc}
    //algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
    //\hline
    //             """)
    algs1.sortBy(_._3 * -1).sortBy(_._2).reverse foreach { case (st, topCount, botCount) =>
      println(s"${st.padTo(10, ' ')} & \t$topCount & \t$botCount \\\\")
    }
    //    algs1.sortBy(_._2).reverse foreach { case (st, topCount, rndCount, botCount) =>
    //      println(s"${st.padTo(10, ' ')} & \t$topCount & \t$rndCount & \t$botCount \\\\")
    //    }
    println(
      """\end{tabular}
      """.stripMargin)
  }
}