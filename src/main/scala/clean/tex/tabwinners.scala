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
import ml.classifiers.{NoLearner, NinteraELM, RF, SVMLibRBF}
import util.Stat

object tabwinners extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   //, "comprimento:all,half,50", "porRisco:r", "dist:euc,man,mah")
   val context = "tabwinnerstex"
   val n = 1
   run()

   override def run() = {
      super.run()
      val measure = ALCKappa
      val ls = learners(learnersStr)
      val datasetLearnerAndBoth = for {
         dataset <- datasets.toList //.par
         l <- ls
      } yield {
         val sts = stratsPool() ++ (l match {
            case _: SVMLibRBF => stratsFpool()
            case _ => stratsFpool().dropRight(2)
         //            case _: NinteraELM => strats0.dropRight(4) ++ strats0.takeRight(2).dropRight(1)
         //            case _: RF => strats0.dropRight(4) ++ strats0.takeRight(1)
         //            case _ => strats0.dropRight(4)
         })
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val (ti, th, tf, tpass) = ranges(ds)
         try {
            val sres = for {
               s0 <- sts
               s = s0(l)
            } yield {
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                     println((ds, s, l, r, f) + ": medida não encontrada")
                     sys.exit(1)
                  }
               s.limpa.split(" ").head -> Stat.media_desvioPadrao(vs.toVector)._1
            }
            val rnd = sres.find(_._1 == RandomSampling(Seq()).limpa).get._2
            Some(ds.dataset + l.abr -> sres.groupBy(_._2).toList.sortBy(_._1).reverse.take(n).map(_._2.map(_._1)).flatten,
               ds.dataset + l.abr -> sres.groupBy(_._2).toList.sortBy(_._1).take(n).map(_._2.map(_._1)).flatten,
               ds.dataset + l.abr -> sres.filter(_._2 <= rnd).map(_._1))
         } catch {
            case e: Throwable => println(s"$e")
               sys.exit(1)
         } finally {
            ds.close()
         }
      }

      val (datasetLearnerAndWinners, datasetLearnerAndLosers, pioresQueRnd) = datasetLearnerAndBoth.flatten.unzip3

      println(datasetLearnerAndWinners)
      println(s"${datasetLearnerAndBoth.size} tests.")
      println(s"--------$measure---------------")
      //      datasetLearnerAndWinners foreach println
      val flat = datasetLearnerAndWinners.flatMap(_._2)
      val flat2 = datasetLearnerAndLosers.flatMap(_._2)
      val flat3 = pioresQueRnd.flatMap(_._2)
      val algs = (for (s <- stratsPool() ++ stratsFpool()) yield s(NoLearner()).limpa) map { st =>
         val topCount = flat.count(_ == st)
         val botCount = flat2.count(_ == st)
         val rndCount = flat3.count(_ == st)
         (st, topCount, rndCount, botCount)
      }

      println( """\begin{tabular}{lccc}
algoritmo & \makecell{primeiros\\lugares} & \makecell{derrotas\\para Rnd}  & \makecell{últimos\\lugares} \\
\hline""")
      algs.sortBy(_._2).reverse foreach { case (st, topCount, rndCount, botCount) =>
         println(s"${st.padTo(10, ' ')} & $topCount & $rndCount & $botCount \\\\")
      }
      println( """\end{tabular}""")

      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(ss.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 100
      //      tbs foreach { case res1 =>
      //        StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ss.toVector.map(_.toString), "nomeTab", measure.toString)
      //      }
   }

}
