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

package exp.other

import ml.models.Model
import ml.neural.elm.EMELM
import util.{Datasets, Tempo}

import scala.collection.mutable
import scala.util.Random

/**
 * Created by davi on 21/05/14.
 */
object ELMTrTimeTsTimeAcc extends App {
  val dataset_path = "/home/davi/wcs/ucipp/uci/"
  if (args.length != 3) {
    println("Usage: ELMTrTimeTsTimeAcc dataset L N")
    sys.exit(0)
  }
  val arff = dataset_path + args(0) + ".arff"
  val rnd = new Random(0)
  val data = rnd.shuffle(Datasets.arff(bina = true)(arff).right.get).take(args(2).toInt)

  val L = args(1).toInt
  val K = 5

  //p/ cada fold, tira 3 medidas (trt, tst, acc) ao longo de uma fila
  def SeqQueue() = Seq.fill(K)(mutable.Queue[(Double, Double, Double)]())

  val measures = Seq.fill(6)(SeqQueue())
  val minSize = Datasets.kfoldCV(data, K, parallel = false) {
    case (tr, ts, k, minSize1) =>
      def ev(list: mutable.Queue[(Double, Double, Double)])(m: Model, trt: Double) {
        val (acc, tst) = Tempo.timev(m.accuracy(ts))
        list += ((trt, tst, acc))
      }

      //I
      val i = IELM(L, k, 1, callf = true, ev(measures(0)(k)))
      i.build(tr)

      //EI
      val ei = EIELM(L, k, 1, callf = true, ev(measures(1)(k)))
      ei.build(tr)

      //ELM
      1 to L foreach { l =>
        val i = OSELM(l, k)
        val (m, trt) = Tempo.timev(i.build(tr))
        val (acc, tst) = Tempo.timev(m.accuracy(ts))
        measures(2)(k) += ((trt, tst, acc))
      }

      //CI
      val ci = CIELM(L, k, 1, ev(measures(3)(k)))
      ci.build(tr)

      //ECI
      val eci = ECIELM(L, k, 1, ev(measures(4)(k)))
      eci.build(tr)

      //EM
      val (em, trt) = Tempo.timev(EMELM(tr, k))
      val (acc, tst) = Tempo.timev(em.accuracy(ts))
      measures(5)(k) += ((trt, tst, acc))
      2 to L foreach { _ =>
        val trt = Tempo.time(em.grow())
        val (acc, tst) = Tempo.timev(em.accuracy(ts))
        measures(5)(k) += ((trt, tst, acc))
      }

      println(minSize1)
      minSize1
    case _ => println("problemas no CV"); sys.exit(0)
  }(0)

  //summarize
  val measuresSummarized = measures.map { methodFolds =>
    val trans = methodFolds.transpose
    val trts = trans.map(_.map(_._1).sum / K)
    val tsts = trans.map(_.map(_._2).sum / K)
    val accs = trans.map(_.map(_._3).sum / K)
    (trts, tsts, accs)
  }

  //trt
  measuresSummarized.map(_._1).transpose.zipWithIndex foreach { case (l, idx) =>
    println((1 + idx) + " " + l.mkString(" "))
//    println(l.mkString(" "))
  }

  //tst
  measuresSummarized.map(_._2).transpose.zipWithIndex foreach { case (l, idx) =>
    println((1 + idx) + " " + l.mkString(" "))
//    println(l.mkString(" "))
  }

  //acc
  measuresSummarized.map(_._3).transpose.zipWithIndex foreach { case (l, idx) =>
    println((1 + idx) + " " + l.mkString(" "))
//    println(l.mkString(" "))
  }

}
