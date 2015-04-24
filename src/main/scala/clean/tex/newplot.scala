///*
//
//active-learning-scala: Active Learning library for Scala
//Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
//*/
//
//package clean.tex
//
//import java.io.PrintWriter
//
//import al.strategies.Passive
//import clean.lib._
//import ml.classifiers.NoLearner
//import util.Stat
//
//import scala.collection.mutable
//
//object newplot extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank {
//   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "porRank:r", "porRisco:r", "dist:euc,man,mah")
//   val context = "newplot"
//   val tipoLearner = "all"
//   val tipoSumariz = "media"
//   val strats = stratsTexRedux(dist)
//   val pioresAignorar = 0
//   val measure = Kappa
//   run()
//
//   override def run() = {
//      super.run()
//      val arq = s"/home/davi/wcs/tese/$measure$pioresAignorar$dist$tipoSumariz$tipoLearner" + (if (porRank) "Rank" else "") + (if (porRisco) "Risco" else "") + ".plot"
//      println(s"$arq")
//      val dss = datasets.take(765675).filter { d =>
//         val ds = Ds(d, readOnly = true)
//         ds.open()
//         val U = ds.poolSize.toInt
//         ds.close()
//         U > 200
//      }
//      val tests = for {
//         dataset <- dss
//         les = if (pioresAignorar == 0) learners(learnersStr)
//         else dispensaMelhores(learners(learnersStr).map { l =>
//            val ds = Ds(dataset, readOnly = true)
//            ds.open()
//            val vs = for (r <- 0 until runs; f <- 0 until folds) yield measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
//            ds.close()
//            l -> Stat.media_desvioPadrao(vs.toVector)._1
//         }, pioresAignorar)(-_._2).map(_._1).par
//         le <- les
//      } yield {
//         println(les.map(_.limp).mkString("   "))
//         val ds = Ds(dataset, readOnly = true)
//         println(s"$ds")
//         ds.open()
//         val (stras, curvas) = (for {
//            s0 <- strats
//         } yield {
//            val s = s0(le)
//            val v25 = for {
//               r <- 0 until runs
//               f <- 0 until folds
//            } yield measure(ds, s, le, r, f)(0).readAll(ds).getOrElse(throw new Error(s"NA: ${(ds, s, le.abr)}"))
//            val plotmedio = v25.transpose.map { v =>
//               if (porRisco) Stat.media_desvioPadrao(v.toVector)._2 * (if (porRank) -1 else 1)
//               else Stat.media_desvioPadrao(v.toVector)._1
//            }
//            val fst = plotmedio.head
//            s.limp -> plotmedio.reverse.padTo(200, fst).reverse.toList //para evitar degraus nos diferentes inicios de cada base
//         }).unzip
//         ds.close()
//         lazy val rank = stras zip (curvas.transpose map ranqueia).transpose
//
//         //cada strat uma curva
//         if (porRank) rank else stras zip curvas
//      }
//
//      //emenda conjuntos de curvas que vieram de vários datasets, agrupa por strat e faz curva media de cada uma
//      //      println(tests.flatten.groupBy(_._1))
//      val (sts, plots) = tests.flatten.groupBy(_._1).map { case (st, v) =>
//         val curvas = v.map(_._2)
//         val curvamedia = curvas.transpose.map(x => Stat.media_desvioPadrao(x.toVector)._2)
//         val curvasuave = curvamedia.sliding(20).map(y => y.sum / y.size).toList
//         st -> curvasuave
//      }.unzip
//      val plot = plots.transpose
//      val fw = new PrintWriter(arq, "ISO-8859-1")
//      fw.write("budget " + sts.mkString(" ") + "\n")
//      plot.zipWithIndex foreach { case (momento, i) =>
//         fw.write((i + 10) + " " + momento.mkString(" ") + "\n")
//      }
//      fw.close()
//      println(s"$arq " + plots.size + " strats.")
//   }
//}
