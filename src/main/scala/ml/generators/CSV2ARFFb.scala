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

import ml.classifiers._
import ml.models.ELMModel
import util.{Datasets, Stat, Tempo}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object CSV2ARFFb extends App {
   val path = "/home/davi/wcs/als/p_results_H3O.csv"
   val (linhaUm, linhas) = Source.fromFile(path).getLines().toList.splitAt(1)
   val tuplas = linhas.map(x => x.split(",").tail.map(_.toDouble) -> x.split(",")(0))
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
   val rnd = new Random(1230)
   val path = "/home/davi/wcs/als/p_results_H3O.csv"
   val patts = Datasets.arff(path + ".arff").right.get
   //   val patts = rnd.shuffle(Datasets.arff(path + ".arff").right.get)
   val hits = Datasets.LOO(patts) { (tr, p) =>
      //      val learner = C45()
      val learner = ninteraELM(rnd.nextInt())
      //      val learner = SVMLib(rnd.nextInt())
      val model = learner.build(tr)
      val model = learner.batchBuild(tr)
      model.hit(p)
   }
   val hitsc = hits.count(_ == true).toDouble
   println(s"acc (${hits.size} exemplos; $hitsc}):${hitsc / hits.size}")
}
