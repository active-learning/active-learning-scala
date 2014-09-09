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

import ml.classifiers.{OSELM, interaELM}
import ml.models.ELMModel
import util.{Datasets, Stat, Tempo}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object CSV2ARFF extends App {
  val linhas = Source.fromFile("/home/davi/wcs/marcos/data.csv").getLines().toList.drop(1)
  val tuplas = linhas.map(x => (x.split(",").drop(2).take(40) :+ x.split(",")(1)).map(_.toDouble).toList)
  //val tuplas = linhas.map(x => (x.split(",").drop(42) :+ x.split(",")(1)).map(_.toDouble).toList)
  tuplas.take(1) foreach println
  val labels = tuplas.map(_.last).distinct.sorted
  println(labels)
  val data = tuplas.map(_.mkString(","))
  val header = List("@relation data") ++ tuplas.head.indices.drop(1).map(i => s"@attribute a$i numeric") ++ List("@attribute class {" + labels.mkString(",") + "}", "@data")
  val pronto = header ++ data
  pronto.take(380) foreach println

  val fw = new FileWriter("/home/davi/wcs/marcos/data-only40superAtts.arff")
  pronto foreach (x => fw.write(s"$x\n"))
  fw.close()
}

object CSV2ARFFCVTest extends App {
  //val patts0 = new Random(1230).shuffle(Datasets.arff(true)("/home/davi/wcs/marcos/data.arff").right.get)
  val patts0 = new Random(1230).shuffle(Datasets.arff("/home/davi/wcs/marcos/data-only40superAtts.arff").right.get)
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilter(patts0, filter)
  val n = patts0.size
  val tr = patts.take(2 * n / 3).grouped(100).toList
  val ts = patts.drop(2 * n / 3)

  7 to 28 by 7 foreach { N =>
    val l = interaELM()
    var mi = l.batchBuild(tr.take(N).flatten).asInstanceOf[ELMModel]
    mi = l.modelSelection(mi)

    println(s"N $N (L ${mi.L})")
    val cv = Datasets.kfoldCV(patts, 10, false) { case (tr0, ts0, fold, minSize) =>
      val tr = new Random(fold).shuffle(tr0).grouped(100).toList
      val ts = ts0
      val os = OSELM(mi.L, fold + 1)
      val (firstm, t) = Tempo.timev(os.build(tr.take(N).flatten))
      val q = mutable.Queue((firstm.accuracy(ts), t))
      tr.drop(N).foldLeft(firstm) { (model, chunk) =>
        val (r, t) = Tempo.timev(chunk.foldLeft(model)((model2, ex) => os.update(model)(ex)))
        q.enqueue((r.accuracy(ts), t))
        r
      }
      q
    }.transpose
    cv.foreach { x =>
      val (m, d, i) = Stat.media_std_intervalo_confianca99(x.map(_._1).toVector)
      println(s"$m $d $i ${x.map(_._2).sum}")
    }
    println("")
  }
}
