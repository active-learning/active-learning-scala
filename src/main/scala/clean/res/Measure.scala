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

package clean.res

import al.strategies.Strategy
import clean.{Blob, CM, Ds}
import ml.classifiers.Learner

trait Measure extends CM with Blob {
   val ds: Ds
   val id: Int
   val s: Strategy
   val l: Learner
   val r: Int
   val f: Int
   val context = "MeaTrait"
   val expectedQtdHits: Int
   lazy val cmsbase = ds.getCMs(s, l, r, f)
   lazy val cms = cmsbase(expectedQtdHits)
   lazy val p = ds.poolId(s, l, r, f)
   val calc: Option[Seq[Double]]

   def qs2hs(qs: Int) = qs - ds.nclasses + 1

   def t2qs(t: Int) = t + 1

   def t2hs(t: Int) = qs2hs(t2qs(t))

   def write() {
      calc match {
         case Some(seq) => ds.write(s"insert into r values ($id, $p, ${doublesTobdString(seq)}")
         case None => ds.log(s"Pool $r.$f incompleto para os hits de $s/$l.")
      }
   }
}

case class balancedAcc(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(t: Int) extends Measure {
   val id = 1
   lazy val prev = {
      ds.readString(s"select vs from r where m=$id and p=$p") match {
         case List(str) => bdstringToDoubles(str)
      }
   }
   lazy val expectedQtdHits = t2hs(t)
   lazy val calc = if (cms.contains(t)) Some(prev ++ Seq(accBal(cms(t)))) else None
}

case class kappa(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(t: Int) extends Measure {
   val id = 2
   lazy val expectedQtdHits = t2hs(t)
   lazy val calc = if (cms.contains(t)) Some(prev ++ Seq(kappa(cms(t)))) else None
}

//
//
// extends CM {
//   val context = "MeaTrait"
//   val budget0: Int
//
//   def id(ds: Ds): Int
//
//   //   def budget(ds: Ds) = math.max(1, math.min(ds.expectedPoolSizes(Global.folds).min, budget0))
//   def budget(ds: Ds) = math.max(ds.nclasses, math.min(ds.expectedPoolSizes(Global.folds).min, budget0))
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int): Double
//}
//
//case class timeToStart(budget0: Int) extends Measure() {
//   def id(ds: Ds) = 1000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      println(s"Tempo não é calculado como as outras medidas. Só o id é necessário.")
//      ???
//   }
//}
//
//case class timeToQuery(budget0: Int) extends Measure() {
//   def id(ds: Ds) = 10000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      println(s"Tempo não é calculado como as outras medidas. Só o id é necessário.")
//      ???
//   }
//}
//
//case class timeToLearn(budget0: Int) extends Measure() {
//   def id(ds: Ds) = 50000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      println(s"Tempo não é calculado como as outras medidas. Só o id é necessário.")
//      ???
//   }
//}
//
//case class ALCaccBal(budget0: Int) extends Measure {
//
//   def id(ds: Ds) = 1300000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
//      val tot = vs.foldLeft(0d)((accBalTot, cm) => accBalTot + accBal(cm))
//      tot / vs.size
//   }
//}
//
//case class accBalAt(budget0: Int) extends Measure() {
//   def id(ds: Ds) = 1400000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = accBal(cms.take(budget(ds) - ds.nclasses + 1).last._2)
//}
//
//case class ALCkappa(budget0: Int) extends Measure {
//   def id(ds: Ds) = 1700000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
//      val tot = vs.foldLeft(0d)((kappaTot, cm) => kappaTot + kappa(cm))
//      tot / vs.size
//   }
//}
//
//case class kappaAt(budget0: Int) extends Measure() {
//   def id(ds: Ds) = 1800000 + budget(ds)
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = kappa(cms.take(budget(ds) - ds.nclasses + 1).last._2)
//}
//
///**
// * medida que só faz sentido para learners C45, NB e 5NN por terem Q=|U|
// */
//case class passiveAccBal() extends Measure() {
//   val budget0 = 0
//
//   def id(ds: Ds) = 1500000
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      accBal(cms(ds.expectedPoolSizes(Global.folds).min - 1))
//   }
//
//}
//
///**
// * Calcula ALC num intervalo de largura 10% do pool (na verdade, 10% das CMs totais possiveis) no limite máximo de 20 de largura.
// * @param step
// */
//case class intervalALCaccBal20or10pct(step: Int) extends Measure {
//   val budget0 = 0
//
//   def id(ds: Ds) = 1600000 + step
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      val stepSize = math.min(20, (ds.expectedPoolSizes(Global.folds).min - ds.nclasses + 1) / 10)
//      val vs = cms.drop(stepSize * step).take(stepSize).values
//      val tot = vs.foldLeft(0d)((accBalTot, cm) => accBalTot + accBal(cm))
//      tot / vs.size
//   }
//}
//
//
////case class ALCgmeans(budget0: Int) extends Measure() {
////   def id(ds: Ds) = 200000 + budget(ds)
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
////      val tot = vs.foldLeft(0d)((gmtot, cm) => gmtot + gmeans(cm))
////      tot / vs.size
////   }
////}
////
////case class accAt(budget0: Int) extends Measure() {
////   def id(ds: Ds) = 1100000 + budget(ds)
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      val acertos = contaAcertos(cms.take(budget(ds) - ds.nclasses + 1).last._2)
////      acertos.toDouble / tsSize
////   }
////}
////
////case class gmeansAt(budget0: Int) extends Measure() {
////   def id(ds: Ds) = 1200000 + budget(ds)
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      gmeans(cms.take(budget(ds) - ds.nclasses + 1).last._2)
////   }
////}
//
////case class costToReachPassiveacc() extends Measure() {
////   val id = 5
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      ???
////   }
////}
////case class costToReachPassiveaccSD() extends Measure() {
////   val id = 7
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      ???
////   }
////}
////case class accAtQSD() extends Measure() {
////   val id = 13
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      ???
////   }
////}
//
//
////case class Q() extends Measure() {
////  val id = 0
////
////  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = ds.Q
////}
//
////case class ALCacc(budget0: Int) extends Measure {
////
////   def id(ds: Ds) = 100000 + budget(ds)
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
////      val acertos = vs.foldLeft(0)((hits, cm) => hits + contaAcertos(cm))
////      acertos.toDouble / (tsSize * vs.size)
////   }
////}
//
//// medidas que só fazem sentido para learners C45, NB e 5NN por terem Q=|U| ---------------------
////case class passiveAcc() extends Measure() {
////   val budget0 = 0
////
////   def id(ds: Ds) = 15
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      acc(cms.last._2)
////   }
////}
////
////
////case class passiveGme() extends Measure() {
////   val budget0 = 0
////
////   def id(ds: Ds) = 16
////
////   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
////      gmeans(cms.last._2)
////   }
////}
////
