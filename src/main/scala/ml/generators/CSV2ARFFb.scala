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

package ml.generators

import java.io.FileWriter

import al.strategies._
import ml.classifiers._
import util.Datasets

import scala.io.Source
import scala.util.Random

object CSV2ARFFb extends App {
   val path = "/home/davi/wcs/als/p_results_H3O.csv"
   val (linhaUm, linhas) = Source.fromFile(path).getLines().toList.splitAt(1)
   val tuplas = linhas.map(x => x.split(",").tail.map(_.toDouble) -> x.split(",")(0).take(5))
   //   tuplas.take(3) foreach println
   val (descs, preds) = tuplas.unzip
   val labels = preds.distinct.sorted
   //   println(preds.size + " " + labels)
   val data = tuplas.map(x => x._1.mkString(",") + "," + x._2)
   val atts = linhaUm.head.split(",").tail
   val header = List(s"@relation ${path.split("/").last}") ++ atts.map(a => s"@attribute $a numeric") ++ List("@attribute class {" + labels.mkString(",") + "}", "@data")
   //   header foreach println
   val pronto = header ++ data
   pronto.take(atts.size + 10) foreach println

   val fw = new FileWriter(path + ".arff")
   fw.write(pronto.mkString("\n"))
   fw.close()
}

object CSV2ARFFbCVTest extends App {
   def accuracy(hits: Seq[Boolean]) = {
      val hc = hits.count(_ == true)
      hc.toDouble / hits.size
   }

   val rnd = new Random(1230)
   val path = "/home/davi/wcs/als/p_results_H3O.csv"
   val patts0 = Datasets.arff(path + ".arff") match {
      case Right(ps) => ps
      case Left(str) => println(s"$str"); sys.exit(1)
   }
   val f = Datasets.zscoreFilter(patts0)

   println(s"t acc aacc")
   1 to patts0.size foreach { toTake =>
      val bothhits = Datasets.LOO(patts0.toArray) { (tr0, p) =>
         val tr = tr0 //Datasets.applyFilter(f)(tr0)

         val learner = KNNBatchb(1, "eucl", tr, weighted = true)
         //         val learner = NBBatch()

         val amostra = Entropy(learner, rnd.shuffle(tr0)).queries.take(toTake)
         val model = learner.build(amostra)

         val aamostra2 = ExpErrorReduction(learner, rnd.shuffle(tr0), "entropy", 35).queries.take(toTake)
         val amodel2 = learner.build(aamostra2)

         //         val aamostra = AgDensityWeightedLabelUtility(tr, "maha").queries.take(toTake)
         //         val amodel = alearner.build(aamostra)

         model.hit(p) -> amodel2.hit(p)
      }
      val (hits, ahits) = bothhits.unzip
      val acc = accuracy(hits)
      val aacc = accuracy(ahits)
      println(s"$acc $aacc")
      //      println(s"$toTake $acc $aacc")
   }
}
