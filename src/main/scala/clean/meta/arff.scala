package clean.meta

import java.io.FileWriter

import clean._
import clean.res.{ALCKappa, BalancedAcc}
import util.Stat

import scala.io.Source

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
object arff extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator {
   val context = "metaAttsApp"
   val arguments = superArguments
   val measure = ALCKappa
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val metadata0 = for {
         name <- datasets.toList
         l <- allLearners().par
         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            val tmp = ranges(ds).take(4) //100 !!!
            ds.close()
            tmp.zipWithIndex.map(x => (x._1._1, x._1._2, x._2))
         }
      } yield {
         val ds = Ds(name, readOnly = true)
         ds.open()
         val seqratts = (for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield ds.attsFromR(r, f)).transpose.map(_.toVector)
         val rattsmd = seqratts map Stat.media_desvioPadrao
         val (rattsm, rattsd) = rattsmd.unzip
         val medidas = for {
            s <- stratsForTree()
         } yield {
            val le = if (s.id >= 17 && s.id <= 21 || s.id == 969) s.learner else l
            val ms = for {
               r <- 0 until Global.runs
               f <- 0 until Global.folds
            } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse(ds.quit(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}."))
            s.abr -> Stat.media_desvioPadrao(ms.toVector)
         }
         val res = (ds.metaAtts ++ Seq(budix.toDouble) ++ rattsm ++ rattsd, l.abr, medidas.maxBy(_._2._1)._1)
         ds.close()
         res
      }
      val metadata = metadata0.toList
      //      metadata foreach println

      //cria ARFF
      val (descAtts, nomAtts, pred) = metadata.unzip3
      val labels = pred.distinct.sorted
      //      println(labels)
      val noms = nomAtts.distinct.sorted
      //      println(noms)
      val data = metadata.map { case (d, n, p) => d.mkString(",") + s",$n,$p"}
      //      val header = List("@relation data") ++ desc.dropRight(1).head.map(i => s"@attribute $i numeric") ++ List("@attribute learner {" + noms.mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
      val numAtts = s"nclasses,nattributes,Uavg,UavgByNatts,nomCount,numCount,nomByNum,lgUavg,lgUavgByNatts,budgetIndex,$attsFromRNames,$attsFromRNamesd"
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute learner {" + allLearners().map(_.abr).mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val fw = new FileWriter("/home/davi/wcs/ucipp/uci/meta.arff")
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()

   }
}