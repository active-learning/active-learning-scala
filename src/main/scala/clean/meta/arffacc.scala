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
import ml.classifiers.ninteraELM
import util.{Datasets, StatTests, Stat}

import scala.io.Source
import scala.util.Random

object arffacc extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator with FilterTrait {
   val ties = false
   val arq = "/home/davi/wcs/ucipp/uci/metaAcc" + (if (ties) "Ties" else "") + ".arff"
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

         (ti, tf) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            val tmp = Seq(maxRange(ds, 2, 100)) // <-verificar trocar p/ 200?
            ds.close()
            tmp
         }

      } yield {
         val ds = Ds(name, readOnly = true)
         ds.open()
         val l = allLearners()(rnd.nextInt(allLearners().size)) //warning: não aceita estrats de learner único (basicamente SVMmulti e Majoritary)
         val seqratts = (for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield ds.attsFromR(r, f)).transpose.map(_.toVector)
         val rattsmd = seqratts map Stat.media_desvioPadrao
         val (rattsm, _) = rattsmd.unzip

         val res = if (ties) {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               val poolStr = (100 * r + f).toString
               val medidas = for {
                  s <- stratsForTree() // <- verificar!!!
               } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                     ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                     -2d
                  }
               poolStr -> medidas
            }
            val winners = StatTests.clearWinners(vs, ss)
            val binario = ss.map(x => if (winners.contains(x)) 1 else 0)
            if (vs.exists(x => x._2.contains(-2d))) None
            else Some(ds.metaAtts ++ rattsm, l.abr, "\"multilabel" + binario.mkString(",") + "\"")
         } else {
            val vs = for {
               s <- stratsForTree() // <- verificar!!!
            } yield {
               val le = if (s.id >= 17 && s.id <= 21 || s.id == 969) s.learner else l
               val ms = for {
                  r <- 0 until Global.runs
                  f <- 0 until Global.folds
               } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                     ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                     -2d
                  }
               s.abr -> Stat.media_desvioPadrao(ms.toVector)
            }
            if (vs.exists(x => x._2._1 == -2d)) None else Some(ds.metaAtts ++ rattsm, l.abr, vs.maxBy(_._2._1)._1)
         }

         ds.close()
         res
      }


      val metadata = metadata0.flatten.toList

      //cria ARFF
      val pred = metadata.map(_._3)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, learner, vencedores) => numericos.mkString(",") + s",$learner,$vencedores"}
      val numAtts = "\"#classes\",\"#atributos\",\"#exemplos\",\"#exemplos/#atributos\",\"%nominais\",\"log(#exs)\",\"log(#exs/#atrs)\"," + attsFromRNames.mkString(",")
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute learner {" + allLearners().map(_.abr).mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      println(s"${data.size}")

      //processa arff
      if (ties) Datasets.arff(arq) match {
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
                     p.nominalSplit(m.predict(p).toInt) == 1
                  }
                  val accELM = hitsELM.count(_ == true) / ftestSet.size.toDouble

                  val hitsRnd = ftestSet.zipWithIndex map { case (p, idx) =>
                     p.nominalSplit(idx % p.nclasses) == 1
                  }
                  val accRnd = hitsRnd.count(_ == true) / ftestSet.size.toDouble

                  val hitsMaj = ftestSet map { p =>
                     p.nominalSplit(2) == 1
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