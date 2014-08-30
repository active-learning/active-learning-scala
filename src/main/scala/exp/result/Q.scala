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

package exp.result

import app.ArgParser
import app.db.entities.Dataset

/**
 * Created by davi on 09/06/14.
 */
object Q extends App {
  val desc = s"Version ${ArgParser.version} \n Calcula, completa dbs e imprime Qs."
  val (path, datasetNames) = ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 3, desc)
  val parallel = args(2) == "y"
  val readOnly = false
  val runs = Dataset("")("").runs
  val folds = Dataset("")("").folds
  val dest = Dataset(path, createOnAbsence = false, readOnly) _
  val qname = (if (parallel) datasetNames.par else datasetNames).toList map { datasetName =>
    val db = dest(datasetName)
    if (db.dbOriginal.exists()) {
      db.open()
      val Q = db.Q
      println(s"$Q $datasetName ${db.n}")
      db.close()
      (Q, datasetName)
    } else (-1, datasetName)
  }
  qname.sortBy(_._1) foreach println
  println("")
  println(qname.sortBy(_._1).map(_._2).mkString(","))
  println("")
}
