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

import al.strategies.{RandomSampling, Strategy}
import clean.{Res, ArgParser, Ds, Exp}
import ml.Pattern
import ml.classifiers._
import weka.filters.Filter

/**
 * Q measure id = 0
 */
trait Measure {
  val id: Int
}

case class ALCacc() extends Measure {
  val id = 1
}

case class ALCgmeans() extends Measure {
  val id = 2
}

case class ALCaccSD() extends Measure {
  val id = 3
}

case class ALCgmeansSD() extends Measure {
  val id = 4
}


case class costToReachPassiveacc() extends Measure {
  val id = 5
}

case class costToReachPassivegmeans() extends Measure {
  val id = 6
}

case class costToReachPassiveaccSD() extends Measure {
  val id = 7
}

case class costToReachPassivegmeansSD() extends Measure {
  val id = 8
}


/**
 * timeToQuery will need extra exp and db table.
 */
case class timeToQuery() extends Measure {
  val id = 9
}

case class timeToQuerySD() extends Measure {
  val id = 10
}

case class accAtQ() extends Measure {
  val id = 11
}

case class gmeansAtQ() extends Measure {
  val id = 12
}

case class accAtQSD() extends Measure {
  val id = 13
}

case class gmeansAtQSD() extends Measure {
  val id = 14
}
