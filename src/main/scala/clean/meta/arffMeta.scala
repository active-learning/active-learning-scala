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

package clean.meta

import java.io.FileWriter

import clean._
import clean.meta.arffTree._
import clean.res.{ALCBalancedAcc, ALCKappa, BalancedAcc}
import ml.classifiers.{KNNBatch, ninteraELM}
import util.{Datasets, StatTests, Stat}

import scala.io.Source
import scala.util.Random

object arffMeta extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator with FilterTrait {
   /*
   "Rank" => prediz ranking das strats
   "Ties" => prediz vencedores empatados
   "Winner" => prediz apenas o melhor
   "Acc" => prediz acc em cada strat

   Escolher mais abaixo se sorteia learner, budget ou nada.
   */
   val modo = "Rank"
   val arq = s"/home/davi/wcs/ucipp/uci/metaAcc$modo.arff"
   val context = "metaAttsAccApp"
   val arguments = superArguments
   val measure = ALCKappa
   //   val measure = ALCBalancedAcc
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val ss = stratsForTree().map(_.abr).toVector
      val metadata0 = for {
         name <- datasets.toList
         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()

            //escolher se sorteia budget
            //            val tmp = Seq(maxRange(ds, 2, 100)) // <-verificar trocar p/ 200?
            //            ds.close()
            //            Seq(tmp._1,tmp._2,0)

            val tmp = ranges(ds, 2, 100) // <- verificar!!! verificar tb argumentos do programa!!!
            ds.close()
            Seq(tmp.zipWithIndex.map(x => (x._1._1, x._1._2, x._2)).apply(rnd.nextInt(2)))

         }
      } yield {
         val ds = Ds(name, readOnly = true)
         println(s"$ds")
         ds.open()

         //escolher se sorteia learner
         //         val l = allLearners()(rnd.nextInt(allLearners().size)) //warning: estrats de learner único permanecem semrpe com seus learners (basicamente SVMmulti e Majoritary)
         val l = KNNBatch(5, "eucl", Seq(), weighted = true)

         val seqratts = (for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield ds.attsFromR(r, f)).transpose.map(_.toVector)
         val rattsmd = seqratts map Stat.media_desvioPadrao
         val (rattsm, _) = rattsmd.unzip

         val res = modo match {
            case "Acc" => //prediz acc em cada strat
               val vs = for {
                  s <- stratsForTree() // <- verificar!!!
               } yield {
                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968) s.learner else l
                  val ms = for {
                     r <- 0 until Global.runs
                     f <- 0 until Global.folds
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  s.abr -> Stat.media_desvioPadrao(ms.toVector)
               }
               if (vs.exists(x => x._2._1 == -2d)) None
               else Some(ds.metaAtts ++ rattsm, l.abr, "\"multilabel" + vs.map(_._2._1).mkString(",") + "\"", if (budix == 0) "baixo" else "alto")
            case "Ties" => //prediz vencedores empatados
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               } yield {
                  val poolStr = (100 * r + f).toString
                  val medidas = for {
                     s <- stratsForTree() // <- verificar!!!
                     le = if (s.id >= 17 && s.id <= 21 || s.id == 968) s.learner else l
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  poolStr -> medidas
               }
               val winners = StatTests.clearWinners(vs, ss)
               val binario = ss.map(x => if (winners.contains(x)) 1 else 0)
               if (vs.exists(x => x._2.contains(-2d))) None
               else Some(ds.metaAtts ++ rattsm, l.abr, "\"multilabel" + binario.mkString(",") + "\"", if (budix == 0) "baixo" else "alto")
            case "Rank" => //prediz ranking
               val vs = for {
                  s <- stratsForTree() // <- verificar!!!
               } yield {
                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968) s.learner else l
                  val ms = for {
                     r <- 0 until Global.runs
                     f <- 0 until Global.folds
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  s.abr -> Stat.media_desvioPadrao(ms.toVector)
               }
               lazy val rank = vs.map(_._2._1).zipWithIndex.sortBy(_._1).map(_._2).zipWithIndex.sortBy(_._1).map(_._2)
               if (vs.exists(x => x._2._1 == -2d)) None
               else Some(ds.metaAtts ++ rattsm, l.abr, "\"multilabel" + rank.mkString(",") + "\"", if (budix == 0) "baixo" else "alto")
            case "Winner" => //prediz apenas o melhor
               val vs = for {
                  s <- stratsForTree() // <- verificar!!!
               } yield {
                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968) s.learner else l
                  val ms = for {
                     r <- 0 until Global.runs
                     f <- 0 until Global.folds
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  s.abr -> Stat.media_desvioPadrao(ms.toVector)
               }
               if (vs.exists(x => x._2._1 == -2d)) None
               else Some(ds.metaAtts ++ rattsm, l.abr, vs.maxBy(_._2._1)._1, if (budix == 0) "baixo" else "alto")
         }

         ds.close()
         res
      }


      val metadata = metadata0.flatten.toList

      //cria ARFF
      val pred = metadata.map(_._3)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, learner, vencedores, budget) => numericos.mkString(",") + s",$budget,$learner,$vencedores"}
      val header = List("@relation data") ++ nonHumanNumAttsNames.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute \"orçamento\" {baixo,alto}", "@attribute learner {" + allLearners().map(_.abr).mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      println(s"${data.size}")

      //processa arff
      if (modo != "Winner") Datasets.arff(arq) match {
         case Left(str) => error("problemas abrindo arff")
         case Right(patterns) =>
            println(s"${patterns.size}")
            val nestedRes = (0 until runs).par map { run =>
               val shuffled = new Random(run).shuffle(patterns)
               Datasets.kfoldCV(shuffled, k = folds) { (tr, ts, fold, minSize) =>
                  //                  println(s"Pool $run.$fold (${tr.size} instances) ...")
                  val learnerSeed = run * 10000 + fold

                  val (fpool, binaf, zscof) = filterTr(tr, fold)
                  val ftestSet = filterTs(ts, fold, binaf, zscof)

                  val m = ninteraELM(learnerSeed).build(fpool)

                  val hitsELM = ftestSet map { p =>
                     println(m.output(p).toList)
                     println(p.nominalSplit.toList)
                     println(s"")
                     p.nominalSplit(m.predict(p).toInt) == p.nominalSplit.max
                  }
                  val accELM = hitsELM.count(_ == true) / ftestSet.size.toDouble

                  val hitsRnd = ftestSet.zipWithIndex map { case (p, idx) =>
                     p.nominalSplit(idx % p.nclasses) == p.nominalSplit.max
                  }
                  val accRnd = hitsRnd.count(_ == true) / ftestSet.size.toDouble

                  val hitsMaj = ftestSet map { p =>
                     p.nominalSplit(2) == p.nominalSplit.max
                  }
                  val accMaj = hitsMaj.count(_ == true) / ftestSet.size.toDouble

                  println(s"$accELM $accMaj")
               }
            }
         //            val res=nestedRes.flatten
         //            res foreach println
      }
   }
}