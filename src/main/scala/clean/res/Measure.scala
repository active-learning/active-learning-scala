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

import clean.{Global, CM, Ds}
import ml.classifiers.{Learner, NB}

import scala.collection.mutable

/**
 * Q measure id = 0
 */
trait Measure extends CM {
   val context = "MeaTrait"
   val budget0: Int

   def id(ds: Ds): Int

   //   def budget(ds: Ds) = math.max(1, math.min(ds.expectedPoolSizes(Global.folds).min, budget0))
   def budget(ds: Ds) = math.max(ds.nclasses, math.min(ds.expectedPoolSizes(Global.folds).min, budget0))

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int): Double
}

//case class Q() extends Measure() {
//  val id = 0
//
//  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = ds.Q
//}

case class ALCacc(budget0: Int) extends Measure {

   def id(ds: Ds) = 100000 + budget(ds)

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
      val acertos = vs.foldLeft(0)((hits, cm) => hits + contaAcertos(cm))
      acertos.toDouble / (tsSize * vs.size)
   }
}

case class ALCaccBal(budget0: Int) extends Measure {

   def id(ds: Ds) = 1300000 + budget(ds)

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
      val tot = vs.foldLeft(0d)((accBalTot, cm) => accBalTot + accBal(cm))
      tot / vs.size
   }
}

case class ALCgmeans(budget0: Int) extends Measure() {
   def id(ds: Ds) = 200000 + budget(ds)

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      val vs = cms.take(budget(ds) - ds.nclasses + 1).values
      val tot = vs.foldLeft(0d)((gmtot, cm) => gmtot + gmeans(cm))
      tot / vs.size
   }
}

case class accAt(budget0: Int) extends Measure() {
   def id(ds: Ds) = 1100000 + budget(ds)

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      val acertos = contaAcertos(cms.take(budget(ds) - ds.nclasses + 1).last._2)
      acertos.toDouble / tsSize
   }
}

case class gmeansAt(budget0: Int) extends Measure() {
   def id(ds: Ds) = 1200000 + budget(ds)

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      gmeans(cms.take(budget(ds) - ds.nclasses + 1).last._2)
   }
}


//case class ALCaccSD() extends Measure() {
//   val id = 3
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class ALCgmeansSD() extends Measure() {
//   val id = 4
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//
//case class costToReachPassiveacc() extends Measure() {
//   val id = 5
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class costToReachPassivegmeans() extends Measure() {
//   val id = 6
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class costToReachPassiveaccSD() extends Measure() {
//   val id = 7
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class costToReachPassivegmeansSD() extends Measure() {
//   val id = 8
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//
///**
// * timeToQuery will need extra exp and db table.
// */
//case class timeToQuery() extends Measure() {
//   val id = 9
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class timeToQuerySD() extends Measure() {
//   val id = 10
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class accAtQSD() extends Measure() {
//   val id = 13
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}
//
//case class gmeansAtQSD() extends Measure() {
//   val id = 14
//
//   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
//      ???
//   }
//}

// medidas que só fazem sentido para learners C45, NB e 5NN por terem Q=|U| ---------------------
case class passiveAcc() extends Measure() {
   val budget0 = 0

   def id(ds: Ds) = 15

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      acc(cms.last._2)
   }
}


case class passiveGme() extends Measure() {
   val budget0 = 0

   def id(ds: Ds) = 16

   def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
      gmeans(cms.last._2)
   }
}
