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

import al.strategies.{Passive, RandomSampling, Strategy}
import clean.lib._
import ml.classifiers._
import util.{Stat, StatTests}

object comparaClassifComAprendiz extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "comparaClassifComAprendiz"
  run()

  override def run() = {
    super.run()
    val measure = Kappa
    val strats = stratsTex("all")
    val out = for {
      dataset <- DsBy(datasets, 200, false).par
      s0 <- strats

    //          lea <- learners(learnersStr)
    //          s = s0(lea)

    } yield {
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        val (ti, th, tf, tpass) = ranges(ds)

        val (accsL, accsC) = (for {
          r <- 0 until runs
          f <- 0 until folds
        } yield {

            //            //melhor lea pelo classif p/ aquela strat
            //            val les = for (le <- learners(learnersStr)) yield {
            //              val s = s0(le)
            //              val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
            //              (classif,
            //                measure(ds, s, classif, r, f)(-2).read(ds).getOrElse(error("NA:" +(ds, s.abr, classif, r, f) + "NA:" +(ds, s.id, classif.id, r, f))),
            //                le)
            //            }
            //            val classif = les.sortBy(_._2).reverse.head._1
            //            val lea = les.sortBy(_._2).reverse.head._3
            //            val s = s0(lea)
            //            (lea.limpa -> measure(ds, s, lea, r, f)(th).read(ds).getOrElse(error("NA:" +(ds, s.abr, lea, r, f) + "NA:" +(ds, s.id, lea.id, r, f))),
            //              classif.limpa -> measure(ds, s, classif, r, f)(-2).read(ds).getOrElse(error("NA:" +(ds, s.abr, classif, r, f) + "NA:" +(ds, s.id, classif.id, r, f))))

            //                        melhor lea pela passiva p/ aquela strat
            val les = for (le <- learners(learnersStr)) yield {
              val s = Passive(Seq())
              (le, measure(ds, s, le, r, f)(-1).read(ds).getOrElse(error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f))), r, f)
            }
            val lea = les.sortBy(_._2).reverse.head._1
            val s = s0(lea)
            val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
            (lea.limpa -> measure(ds, s, lea, r, f)(th).read(ds).getOrElse(error("NA:" +(ds, s.abr, lea, r, f) + "NA:" +(ds, s.id, lea.id, r, f))),
              classif.limpa -> measure(ds, s, classif, r, f)(-2).read(ds).getOrElse(error("NA:" +(ds, s.abr, classif, r, f) + "NA:" +(ds, s.id, classif.id, r, f))))

            //                        //melhor lea p/ aquela strat
            //                        val les = for (le <- learners(learnersStr)) yield {
            //                          val s = s0(le)
            //                          (le, measure(ds, s, le, r, f)(th).read(ds).getOrElse(error("NA:" +(ds, s.abr, le, r, f) + "NA:" +(ds, s.id, le.id, r, f))), r, f)
            //                        }
            //                        val lea = les.sortBy(_._2).reverse.head._1
            //                       val s = s0(lea)
            //                        val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
            //                        (lea.limpa -> measure(ds, s, lea, r, f)(th).read(ds).getOrElse(error("NA:" +(ds, s.abr, classif, r, f) + "NA:" +(ds, s.id, classif.id, r, f))),
            //                          classif.limpa -> measure(ds, s, classif, r, f)(-2).read(ds).getOrElse(error("NA:" +(ds, s.abr, classif, r, f) + "NA:" +(ds, s.id, classif.id, r, f))))
          }).unzip
        val ers: Double = accsL.zip(accsC).count { case (l, c) => l._2 > c._2 }
        val ces: Double = accsL.zip(accsC).count { case (l, c) => c._2 > l._2 }
        val r = if (ers > ces) -1
        else if (ers == ces) 0
        else 1
        //        println(Stat.wilcoxon(accsL zip accsC))
        println(r)
        ("" -> ces) -> ("" -> ers)


        //        //descobre acc do best aprendiz (tem sido só um por pool)
        //        val hists = accsPerPool(ds, s0, learners(learnersStr), (s: Strategy, l: Learner, r: Int, f: Int) => measure(ds, s, l, r, f)(th))
        //        val tupAprendizes = hists map (hist => pegaMelhores(hist, 1)(_._2).head)
        //        val accClassifs = tupAprendizes map { case (lea, v, r, f) =>
        //          val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s0(lea))
        //          classif.limpa -> measure(ds, s0(lea), classif, r, f)(-2).read(ds).getOrElse(error("NA:" +(ds, s0(lea).abr, classif, r, f) + "NA:" +(ds, s0(lea).id, classif.id, r, f)))
        //        }
        //        val accAprendizes = tupAprendizes map { case (lea, v, r, f) => lea.limpa -> v }
        //        ds.close()
        //        val z = accClassifs zip accAprendizes
        //        println(s"${z.count { case (a, b) => (a._1 == b._1 && (b._2 - a._2) == 0.05) }}")

        //descobre best classifs pra Rnd Samp (gravei um por pool na base)
        //         val classifs = for {
        //            r <- 0 until runs
        //            f <- 0 until folds
        //         } yield BestClassifCV100_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))

        //descobre best 'aprendizes'(hiters) do Rnd (tem sido só um por pool)
        //         val hists2 = accsPerPool(ds, (lean:Learner)=>RandomSampling(Seq()), learners(learnersStr), (s: Strategy, l: Learner, r: Int, f: Int) => measure(ds, s, l, r, f)(ti, tf))
        //         val aprendizes2 = hists2 map (hist => pegaMelhores(hist, 1)(_._2).head)
        //         val comp = aprendizes.map(_._1.limpa).zip(aprendizes2.map(_._1.limpa)).groupBy(x => x).map(x => x._1 -> x._2.size).toList.sortBy(_._2).reverse


        //        val comp = tupAprendizes.zip(accClassifs).groupBy(x => x).map(x => x._1 -> x._2.size).toList.sortBy(_._2).reverse
        //        //                  comp.maxBy(_._2)._2 -> ("\n" + dataset + s" / ${s0(NBBatch()).limp}:\n" + comp.map(x => x._1 + ":" + x._2).mkString("\t"))
        //
        //        comp.maxBy(_._2)._2 -> comp.filter { case (ss, i) => ss._1 == ss._2 }.map(_._2).sum
      }
    //    //            out.toList.sortBy(_._1).reverse.map(_._2) foreach println
    //    //      out.toList.sortBy(_._2).reverse.map(_._2) foreach println
    //    println(out.toList.sortBy(_._2).reverse.map(_._2).sum / (1650 * 25d))
//    println(Stat.wilcoxon(out.toList))
  }
}

/*
1650:
classif
1418
25
207

best lea
106
49
1495

passivo
905
100
645

9900:
todos
7570
233
2097

 */