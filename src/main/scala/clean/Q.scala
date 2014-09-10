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

package clean

import util.Datasets

import scala.io.Source
import scala.util.Random

object Q extends AppWithUsage {
  val arguments = List("file-with-dataset-names")
  init()

  val datasets = Source.fromFile(args(0)).getLines().filter(_.length > 2)

  datasets foreach { dataset =>
    println(s"Processing dataset $dataset ...")
    val ds = Ds("/home/davi/wcs/ucipp/uci")(dataset)
    val shuffled = new Random(0).shuffle(ds.patterns)
    //    Datasets.kfoldCV()
  }
}
