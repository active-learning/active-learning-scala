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
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "tabwinnersPares"
  val n = 1
  val qs = "100"
  // 50 100 u2
  val measure = ALCKappa
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    //      val strats = Seq(
    //         //         MarginFixo(RF(), Seq()),
    //         HTUFixo(Seq(), RF(), Seq(), "eucl"),
    //         DensityWeightedTrainingUtilityFixo(Seq(), RF(), Seq(), "eucl"),
    //         AgDensityWeightedTrainingUtility(Seq(), "eucl")
    //         //         RandomSampling(Seq())
    //      )
    //    val strats = (for {l <- ls; s <- stratsTex("all").map(_(l))} yield s).distinct
    //    val strats = (for {s <- stratsTex("maha").map(_(RF()))} yield s).distinct
    val strats = (for {s <- stratsTex("maha")} yield s).distinct
    val datasetLearnerAndBoth = for {
      dataset <- datasets.toList
    //        .filter { dataset =>
    //        val ds = Ds(dataset, readOnly = true)
    //        ds.open()
    //        val r = ds.poolSize >= (if (qs == "50") 100 else 200)
    //        ds.close()
    //        if (qs == "u2") !r else r
    //      }
    } yield {
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        //         lazy val (ti, th, tf, tpass) = ranges(ds)
        val sres = for {s0 <- strats} yield {

          val metads = new Db("meta", readOnly = false)
          metads.open()
          val sql = s"select pre from e where mc='ELM' and st='${s0(NoLearner()).limp}' and ds='$dataset'"
          val classif = metads.readString(sql) match {
            case List(Vector(predito)) =>
              val cla = predito.split("-").last
              ls.find(_.limp == cla).getOrElse(???)
            case x => println(s"${x} <- x")
              println(s"${sql} <- ")
              sys.exit(1)
          }
          val s = s0(classif)
          metads.close()

          val (cs, vs) = (for {
            r <- 0 until runs
            f <- 0 until folds
          } yield {
              try {

                //                val classif = RF()
                val (ti, th, tf, tpass) = ranges(ds)
                classif.limpa -> measure(ds, s, classif, r, f)(ti, tf).read(ds).getOrElse {
                  println((ds, s, s.learner, classif, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
                }

                //                val (classif, nr) = qs match {
                //                  case "100" => BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s) -> -2
                //                  case "50" => BestClassifCV50_10foldReadOnlyKappa(ds, r, f, s) -> -3
                //                  case "u2" => BestClassifCVU2_10foldReadOnlyKappa(ds, r, f, s) -> -4
                //                }
                //                classif.limpa -> measure(ds, s, classif, r, f)(nr).read(ds).getOrElse {
                //                  println((ds, s, s.learner, classif, r, f) + ": medida não encontrada")
                //                  sys.exit(0) //NA
                //                }

              } catch {
                case e: Throwable => println((ds, s, s.learner, r, f) + e.getMessage)
                  sys.exit(0) //NA
              }
            }).unzip
          //            if (vs.contains(NA)) None else Some(s.limpa + cs.mkString(";") -> Stat.media_desvioPadrao(vs.toVector)._1)
          s.limpa -> Stat.media_desvioPadrao(vs.toVector)._1
        }
        val rnd = sres.find(_._1 == RandomSampling(Seq()).limp).getOrElse("" -> 0d)._2
        val res = (ds.dataset -> pegaMelhores(sres, n)(_._2).map(_._1),
          ds.dataset -> pegaMelhores(sres, n)(-_._2).map(_._1),
          ds.dataset -> sres.filter(_._2 <= rnd).map(_._1).toList)
        ds.close()
        res
      }

    val (datasetLearnerAndWinners, datasetLearnerAndLosers, pioresQueRnd) = datasetLearnerAndBoth.unzip3
    println(s"$n primeiros/últimos")
    println(s"${datasetLearnerAndBoth.size} tests.")
    println(s"--------$measure---------------")
    val flat = datasetLearnerAndWinners.flatMap(_._2)
    val flat2 = datasetLearnerAndLosers.flatMap(_._2)
    val flat3 = pioresQueRnd.flatMap(_._2)
    val algs1 = strats.map(_(NoLearner()).limp) map { st =>
      val topCount = flat.count(_ == st)
      val botCount = flat2.count(_ == st)
      val rndCount = flat3.count(_ == st)
      (st, topCount, rndCount, botCount)
    }

    println(s"${if (qs == "50") "50" else ""}")
    println( """\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
\hline
             """)
    algs1.sortBy(_._2).reverse foreach { case (st, topCount, rndCount, botCount) =>
      println(s"${st.padTo(10, ' ')} & \t$topCount & \t$rndCount & \t$botCount \\\\")
    }
    println(
      """\end{tabular}
      """.stripMargin)
  }
}