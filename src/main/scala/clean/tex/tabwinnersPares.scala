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

import clean.lib._
import util.{Stat, StatTests}

object tabwinnersPares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "tabwinnersPares"
   val n =3
   run()

   override def run() = {
      super.run()
      //            val measure = ALCBalancedAcc
      val measure = ALCKappa
      val strats = stratsForTreeRedux().dropRight(7)
      val ls = learners(learnersStr)
      val algs = (for (s <- strats; le <- ls) yield s.limpa + le.limpa).toVector

      val datasetLearnerAndBoth = for {
         dataset <- datasets.toList.par
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val (ti, th, tf, tpass) = ranges(ds)
         try {
            val sres = for {
               s <- strats
               l <- ls
            } yield {
               val le = l //if (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000 || s.id == 1006600 || s.id == 1292212) s.learner else l
               val vs = for {
                     r <- 0 until runs
                     f <- 0 until folds
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        println((ds, s, le, r, f) + ": medida não encontrada")
                        NA
                     }
               (s.limpa -> l.limpa) -> Stat.media_desvioPadrao(vs.toVector)._1
            }
            Some(ds.dataset -> sres.groupBy(_._2).toList.sortBy(_._1).reverse.take(n).map(_._2.map(_._1)).flatten, ds.dataset -> sres.groupBy(_._2).toList.sortBy(_._1).take(n).map(_._2.map(_._1)).flatten)

            //            val vs = for {
            //               r <- 0 until runs
            //               f <- 0 until folds
            ////               multiplicadorDeAmostra <- 0 to 7
            //            } yield {
            //               val poolStr = (100 * r + f).toString
            //               val sres = for {
            //                  s <- strats
            //                  l <- ls
            //               } yield {
            //                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968 || s.id == 969) s.learner else l
            //                  val (ti, th, tf, tpass) = ranges(ds)
            //                  measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(-2d)
            //               }
            //               poolStr -> sres
            //            }
            ////            Some((ds.dataset + l.toString.take(3)) -> StatTests.winners(vs, ss), (ds.dataset + l.toString.take(3)) -> StatTests.losers(vs, ss))
            //            Some(ds.dataset -> StatTests.winners(vs, algs), ds.dataset -> StatTests.losers(vs, algs))

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
      val algs1 = strats.map(_.limpa) map { st =>
         val topCount = flat.count(_._1 == st)
         val botCount = flat2.count(_._1 == st)
         (st, topCount, botCount)
      }
      val algs2 = algs map { stle =>
         val topCount = flat.count(x => x._1 + x._2 == stle)
         val botCount = flat2.count(x => x._1 + x._2 == stle)
         (stle, topCount, botCount)
      }
      println(algs1.map(_._2).sum + " total de vencedores")
      println(algs1.map(_._3).sum + " total de perdedores")
      algs1.sortBy(_._2).reverse.foreach { case (st, topCount, botCount) =>
         println(s"${st.padTo(10, ' ')}:\t$topCount\taparições entre os $n primeiros;\t\t$botCount\taparições entre os $n últimos")
      }
      println(s"------------------------------")
      println(s"")
      println(s"")
      println(s"")
      println(algs2.map(_._2).sum + " total de 1fst places")
      println(algs2.map(_._3).sum + " total de last places")
      algs2.sortBy(_._2).reverse.foreach { case (st, topCount, botCount) =>
         println(s"${st.padTo(10, ' ')}:\t$topCount\t1st places;\t\t$botCount\tlast places")
      }
      println(s"------------------------------")
      println(s"")
      println(s"")
      println(s"")

      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(ss.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 100
      //      tbs foreach { case res1 =>
      //        StatTests.extensiveTable2(res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ss.toVector.map(_.toString), "nomeTab", measure.toString)
      //      }
   }

}
