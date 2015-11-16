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
import ml.classifiers.NoLearner
import util.Stat

object plot extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah")
  val context = "plot"
  val tipoLearner = "all"
  val tipoSumariz = "media"
  val measure = Kappa
  val conf = "tese"
  //  val conf = "teseilustraall"
  val strats = conf match {
    case "artigos/hais14-expandido" => stratsTexRedux(dist)
    case "artigos/bracis15" => stratsForBRACIS15
    case "tese" => stratsTexForGraficoSimples
    case "teseilustra" => stratsTexForGraficoSimplesIlustra
    case "teseilustraall" => stratsTexForGraficoSimplesIlustra
  }
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    val ls2 = tipoLearner match {
      case "best" | "mediano" => Seq(NoLearner())
      case "all" => ls
    }
    //    val arq = s"/home/davi/wcs/$conf/${ls2.map(_.limp).mkString}$measure$dist$tipoSumariz$tipoLearner$bina" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".tex"
    val arq = s"/home/davi/wcs/$conf/$learnerStr-" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + s"$tipoSumariz.tex"
    println(s"$arq")
    println(s"${ls2} <- ls2")
    println(datasets.size)
    val (sls0, res9) = (for {
      dataset <- datasets
      le0 <- ls2.par
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"$ds")
        ds.open()
        val le = tipoLearner match {
          //          case "mediano" => ls.map { l =>
          //            val vs = for (r <- 0 until runs; f <- 0 until folds) yield measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit(" passiva não encontrada"))
          //            l -> Stat.media_desvioPadrao(vs.toVector)._1
          //          }.sortBy(_._2).apply(ls.size / 2)._1
          //          case "best" => ls.map { l =>
          //            val vs = for (r <- 0 until runs; f <- 0 until folds) yield measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit(" passiva não encontrada"))
          //            l -> Stat.media_desvioPadrao(vs.toVector)._1
          //          }.maxBy(_._2)._1
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
              } yield measure(ds, s, le, r, f)(-2).readAll99(ds)
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
              ts.reverse.padTo(100, fst).reverse.toList
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
      x.sliding(5).map(y => y.sum / y.size).toList
    }.transpose

    val fw = new PrintWriter(arq, "UTF-8")
    fw.write("budget " + sls.map(_.limp).mkString(" ") + "\n")
    plot.zipWithIndex foreach { case (re, i) =>
      fw.write((i + 2) + " " + re.map(_ / (ls2.size * datasets.size)).mkString(" ") + "\n")
    }
    fw.close()
    println(s"$arq " + (res0.size / ls2.size.toDouble) + " datasets completos.")
  }
}
