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

import clean.CM

/**
 * Q measure id = 0
 */
trait Measure {
  val id: Int

  def calc(cms: List[Array[Array[Int]]], total: Int): Double
}

case class ALCacc() extends Measure with CM {
  val id = 1

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    val acertos = cms.foldLeft(0)((hits, cm) => hits + contaAcertos(cm))
    acertos.toDouble / total
  }
}

case class ALCgmeans() extends Measure {
  val id = 2

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class ALCaccSD() extends Measure {
  val id = 3

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class ALCgmeansSD() extends Measure {
  val id = 4

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}


case class costToReachPassiveacc() extends Measure {
  val id = 5

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class costToReachPassivegmeans() extends Measure {
  val id = 6

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class costToReachPassiveaccSD() extends Measure {
  val id = 7

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class costToReachPassivegmeansSD() extends Measure {
  val id = 8

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}


/**
 * timeToQuery will need extra exp and db table.
 */
case class timeToQuery() extends Measure {
  val id = 9

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class timeToQuerySD() extends Measure {
  val id = 10

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class accAtQ() extends Measure {
  val id = 11

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class gmeansAtQ() extends Measure {
  val id = 12

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class accAtQSD() extends Measure {
  val id = 13

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}

case class gmeansAtQSD() extends Measure {
  val id = 14

  def calc(cms: List[Array[Array[Int]]], total: Int) = {
    ???
  }
}
