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

import al.strategies.Passive
import clean.lib._
import util.{Stat, StatTests}

object tabwinnersLearners extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "tabwinnersLearners"
   val n = 1
   val measure = Kappa
   run()

   override def run() = {
      super.run()
      val ls = learners(learnersStr)

      val datasetLearnerAndBoth = for {
         dataset <- datasets.toList.par
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         try {
            val sres = for {
               l <- ls
            } yield {
               val le = l //if (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000 || s.id == 1006600 || s.id == 1292212) s.learner else l
               val vs = for {
                     r <- 0 until runs
                     f <- 0 until folds
                  } yield measure(ds, Passive(Seq()), le, r, f)(-1).read(ds).getOrElse {
                        println((ds, Passive(Seq()), le, r, f) + ": medida não encontrada")
                        NA
                     }
               l.limpa -> Stat.media_desvioPadrao(vs.toVector)._1
            }
            Some(ds.dataset -> pegaMelhores(sres, n)(_._2).map(_._1), ds.dataset -> pegaMelhores(sres, n)(-_._2).map(_._1))
         } catch {
            case e: Throwable => println(s"$e")
               sys.exit(1) //None
         } finally {
            ds.close()
         }
      }

      val (datasetLearnerAndWinners, datasetLearnerAndLosers) = datasetLearnerAndBoth.flatten.unzip
      println(s"$n primeiros/últimos")
      println(s"${datasetLearnerAndBoth.size} tests.")
      println(s"--------$measure---------------")
      //      datasetLearnerAndWinners foreach println
      val flat = datasetLearnerAndWinners.flatMap(_._2)
      val flat2 = datasetLearnerAndLosers.flatMap(_._2)
      val algs1 = ls.map(_.limpa) map { le =>
         val topCount = flat.count(_ == le)
         val botCount = flat2.count(_ == le)
         (le, topCount, botCount)
      }
      println(algs1.map(_._2).sum + " total de vencedores")
      println(algs1.map(_._3).sum + " total de perdedores")
      println(algs1.sortBy(_._2).reverse.map(x => x._1).mkString(" & "))
      algs1.sortBy(_._2).reverse.foreach { case (st, topCount, botCount) =>
         println(s"${st.padTo(10, ' ')} & \t$topCount\t & \t\t$botCount\t\\\\ ")
      }
      println(s"------------------------------")

      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(ss.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 100
      //      tbs foreach { case res1 =>
      //        StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ss.toVector.map(_.toString), "nomeTab", measure.toString)
      //      }
   }

}

/*
--------Kappa---------------
78 total de vencedores
76 total de perdedores
SVM & C4.5 & 5NN & NB
SVM        & 	36	 & 		5	\\
C4.5       & 	17	 & 		11	\\
5NN        & 	15	 & 		29	\\
NB         & 	10	 & 		31	\\
------------------------------
--------Kappa---------------
79 total de vencedores
81 total de perdedores
SVM & C4.5 & 5NN & NB & VFDTw
SVM        & 	33	 & 		2	\\
C4.5       & 	17	 & 		8	\\
5NN        & 	15	 & 		18	\\
NB         & 	10	 & 		22	\\
VFDTw      & 	4	 & 		31	\\
------------------------------
*/