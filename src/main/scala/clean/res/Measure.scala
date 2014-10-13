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

import scala.collection.mutable

/**
 * Q measure id = 0
 */
trait Measure extends CM {
  val id: Int
  val context = "MeaTrait"

  def check(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]]) = {
    if (ds.Q - ds.nclasses + 1 != cms.size) ds.error(s"${ds.Q - ds.nclasses + 1} differs from ${cms.size}")
    if (cms.last._1 != ds.Q - 1) ds.error(s"${cms.last._1} differs from ${ds.Q - 1}")
  }

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int): Double
}

case class Q() extends Measure() {
  val id = 0

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = ds.Q
}

case class ALCacc() extends Measure {
  val id = 1

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    check(ds, cms)
    val acertos = cms.values.foldLeft(0)((hits, cm) => hits + contaAcertos(cm))
    acertos.toDouble / (tsSize * cms.size)
  }
}

case class ALCgmeans() extends Measure() {
  val id = 2

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    ???
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

case class accAtQ() extends Measure() {
  val id = 11

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    check(ds, cms)
    val acertos = contaAcertos(cms.last._2)
    acertos.toDouble / tsSize
  }
}

case class gmeansAtQ() extends Measure() {
  val id = 12

  def calc(ds: Ds, cms: mutable.LinkedHashMap[Int, Array[Array[Int]]], tsSize: Int) = {
    check(ds, cms)
    gmeans(cms.last._2)
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
