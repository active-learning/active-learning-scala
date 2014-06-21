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

import ml.neural.elm.CEOSELM
import util.{Datasets, Tempo}

import scala.util.Random

object ARFF2matlab extends App {
  val rnd = new Random(0)
  val data = Datasets.arff(bina = true)("/home/davi/working-copies/arff/abalone-11classes.arff").right.get
  rnd.shuffle(data).drop(2000) foreach {
    x => println(x.label + "," + x.toString_without_class)
  }
  1 to 10 foreach {
    _ =>
      Tempo.start
      val ceos = CEOSELM(0)
      ceos.build(data.take(30))
      ceos.grow(20)
      data.take(2000).drop(30) foreach ceos.increment
      print(ceos.accuracy(data.drop(2000)))
      Tempo.print_stop
  }
}
