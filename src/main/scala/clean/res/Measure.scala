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

import clean.{CM, Ds}
import ml.classifiers.{Learner, NB}

import scala.collection.mutable

/**
 * Q measure id = 0
 */
trait Measure extends CM {
  val id: Int
  val context = "MeaTrait"

  //  def check(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]]) = {
  //    if (ds.Q - ds.nclasses + 1 != cms.size) ds.error(s"${ds.Q - ds.nclasses + 1} differs from ${cms.size}")
  //    if (cms.last._1 != ds.Q - 1) ds.error(s"${cms.last._1} differs from ${ds.Q - 1}")
  //  }

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int): Double
}

//case class Q() extends Measure() {
//  val id = 0
//
//  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = ds.Q
//}

case class ALCacc(maxBudget: Int) extends Measure {
  val id = 100000 + maxBudget

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    val acertos = cms.take(maxBudget - ds.nclasses + 1).values.foldLeft(0)((hits, cm) => hits + contaAcertos(cm))
    acertos.toDouble / (tsSize * (maxBudget - ds.nclasses + 1))
  }
}

case class ALCgmeans(maxBudget: Int) extends Measure() {
  val id = 200000 + maxBudget

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    val tot = cms.take(maxBudget - ds.nclasses + 1).values.foldLeft(0d)((gmtot, cm) => gmtot + gmeans(cm))
    tot.toDouble / (maxBudget - ds.nclasses + 1)
  }
}

case class ALCaccSD() extends Measure() {
  val id = 3

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class ALCgmeansSD() extends Measure() {
  val id = 4

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}


case class costToReachPassiveacc() extends Measure() {
  val id = 5

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class costToReachPassivegmeans() extends Measure() {
  val id = 6

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class costToReachPassiveaccSD() extends Measure() {
  val id = 7

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class costToReachPassivegmeansSD() extends Measure() {
  val id = 8

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}


/**
 * timeToQuery will need extra exp and db table.
 */
case class timeToQuery() extends Measure() {
  val id = 9

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class timeToQuerySD() extends Measure() {
  val id = 10

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class accAt(budget: Int) extends Measure() {
  val id = 1100000 + budget

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    val acertos = contaAcertos(cms.take(budget - ds.nclasses + 1).last._2)
    acertos.toDouble / tsSize
  }
}

case class gmeansAt(budget: Int) extends Measure() {
  val id = 1200000 + budget

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    gmeans(cms.take(budget - ds.nclasses + 1).last._2)
  }
}

case class accAtQSD() extends Measure() {
  val id = 13

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class gmeansAtQSD() extends Measure() {
  val id = 14

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
  }
}

case class passiveAcc() extends Measure() {
  val id = 15

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    acc(cms.last._2)
  }
}

case class passiveGme() extends Measure() {
  val id = 16

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    gmeans(cms.last._2)
  }
}
