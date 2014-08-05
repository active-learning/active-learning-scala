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

import java.io.{File, FileWriter}

import app.ArgParser
import ml.classifiers.{interaELM, OSELM}
import util.{Tempo, Datasets}
import weka.core.Instances

import scala.io.Source
import scala.util.Random

object CSV2ARFF extends App {
  val csv = Source.fromFile("/home/davi/wcs/marcos/data.csv").getLines().toList.drop(1)
  val tuplas = csv.map(_.split(",").drop(1).map(_.toDouble))
  val labelcerto = tuplas.map(_.reverse)
  val labels = labelcerto.map(_.last).distinct.sorted
  val data = labelcerto.map(_.mkString(","))
  val header = List("@relation data") ++ labelcerto.head.indices.drop(1).map(i => s"@attribute a$i numeric") ++ List("@attribute class {" + labels.mkString(",") + "}", "@data")
  val pronto = header ++ data
  pronto.take(380) foreach println

  val fw = new FileWriter("/home/davi/wcs/marcos/data.arff")
  pronto foreach (x => fw.write(s"$x\n"))
  fw.close()
}

object CSV2ARFFTest extends App {
  val patts0 = new Random(1230).shuffle(Datasets.arff(true)("/home/davi/wcs/marcos/data.arff").right.get)
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilter(patts0, filter)
  val n = patts0.size
  val tr = patts.take(2 * n / 3).grouped(100).toList
  val ts = patts.drop(2 * n / 3)

  val l = OSELM(12)
  Tempo.timev(l.build(patts.take(2 * n / 3)))
  val (firstm, t) = Tempo.timev(l.build(patts.take(2 * n / 3)))
  //  val (firstm, t) = Tempo.timev(l.build(tr.head))
  println(firstm.accuracy(ts) + " " + t * 1000)
  println("")
  val m = tr.drop(1).foldLeft(firstm) { (model, chunk) =>
    val (r, t) = Tempo.timev(chunk.foldLeft(model)((model2, ex) => l.update(model)(ex)))
    println(r.accuracy(ts) + " " + t * 1000) //+ " " + r.L)
    r
  }
  println("")

  val a = {
    val l = OSELM(12)
    val (firstm, t) = Tempo.timev(l.build(tr.head))
    println(firstm.accuracy(ts) + " " + t * 1000)
    val m = tr.drop(1).foldLeft(firstm) { (model, chunk) =>
      val (r, t) = Tempo.timev(chunk.foldLeft(model)((model2, ex) => l.update(model)(ex)))
      println(r.accuracy(ts) + " " + t * 1000) //+ " " + r.L)
      r
    }
  }
}
